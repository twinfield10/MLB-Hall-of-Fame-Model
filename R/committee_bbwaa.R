# =============================================================================
# BBWAA committee model.
#
# A separate, committee-specific model that predicts the Baseball Writers'
# (BBWAA) vote — distinct from the any-route `inducted` model, which lumps every
# election body together. It is selected via `model.committee: bbwaa` and leaves
# the any-route path byte-identical when off.
#
# Design (see docs/MODELING.md):
#   * Unit of analysis = the player-ballot-year panel (R/ballot_features.R).
#   * ONE model: a vote-SHARE regressor (share = votes/ballots), trained with
#     grouped CV by ballot year so a whole cycle is held out at once (prior-year
#     share makes random k-fold leak).
#   * Both requested outputs come from that one leak-free model:
#       - pred_share       : projected ballot vote %
#       - P_elected_cycle  : P(elected this cycle)     ] Monte-Carlo forward
#       - P_eventual_bbwaa : P(ever elected by BBWAA)   ] simulation (below)
#
# Why simulation rather than a separate "elected"/"eventual" classifier: election
# is rare per row and an eventual-outcome classifier's trajectory features would
# leak the election-year share. Rolling the share regressor forward — re-feeding
# each simulated year's share as next year's prev_share, applying the 75% election
# / 5% survival / era ballot-tenure rules, resampling empirical OOF residuals for
# the noise — is leak-free and makes P_eventual >= P_elected_cycle by construction.
# =============================================================================

suppressMessages({ library(caret); library(dplyr); library(doParallel) })

if (!exists("build_ballot_panel")) source(file.path("R", "ballot_features.R"))
if (!exists(".ENGINES"))           source(file.path("R", "model.R"))
if (!exists("auc_roc"))            source(file.path("R", "evaluate.R"))

`%notin%` <- Negate(`%in%`)
`%||%`    <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Predictors = everything except the identifiers and the non-modelled target.
.BBWAA_SHARE_FORMULA <- stats::as.formula(share ~ . - playerID - yearID - elected)

# Ballot-dynamic features updated each simulated year (all numeric, no factors).
.BBWAA_DYN <- c("appearance_number", "is_first_ballot", "years_remaining",
                "prev_share", "prev2_share", "prev_delta", "max_share_prior")

# Rule-of-10 ballot-strength features (added when bbwaa.use_ballot_strength is on).
.BBWAA_STRENGTH <- c("competitor_mass", "n_cand", "n_strong_competitors")
.strip_strength_if_off <- function(df, cfg) {
  if (isFALSE(cfg$bbwaa$use_ballot_strength))
    df <- dplyr::select(df, -dplyr::any_of(.BBWAA_STRENGTH))
  df
}

# ---- data shaping -----------------------------------------------------------

#' Zero-fill the lagged ballot features (NA on a player's first appearance); the
#' is_first_ballot flag carries the "no history" signal so 0 is not ambiguous.
.fill_ballot_na <- function(df) {
  df %>% dplyr::mutate(dplyr::across(
    dplyr::any_of(c("prev_share", "prev2_share", "prev_delta", "max_share_prior")),
    ~ dplyr::coalesce(as.numeric(.), 0)))
}

#' Training frame: every historical ballot-year row for this player type. The
#' inner join to the (POS-filtered) merit matrix routes batters vs pitchers.
build_bbwaa_train <- function(panel, merit, cfg) {
  banned <- unlist(cfg$cohorts$banned_ids)
  panel %>%
    dplyr::filter(playerID %notin% banned) %>%
    dplyr::inner_join(merit, by = "playerID") %>%
    .fill_ballot_na() %>%
    .strip_strength_if_off(cfg)
}

#' Score frame: one row per candidate on the cycle's ballot, with ballot state
#' entering the cycle (holdovers use their real trajectory; first-timers default
#' to appearance 1 / no prior share).
build_bbwaa_score <- function(panel, merit, cohort_ids, cfg) {
  season <- cfg$season
  cohort <- data.frame(playerID = unique(cohort_ids), stringsAsFactors = FALSE)
  st     <- ballot_state_asof(panel, season)

  base <- cohort %>%
    dplyr::left_join(st, by = "playerID") %>%
    dplyr::mutate(
      is_first_ballot   = ifelse(is.na(n_prior), 1L, as.integer(is_first_ballot)),
      appearance_number = ifelse(is.na(appearance_number), 1L, appearance_number),
      years_remaining   = bbwaa_ballot_max_years(season) - appearance_number
    ) %>%
    dplyr::select(-dplyr::any_of("n_prior"))

  # Ballot strength across the FULL cohort — batters and pitchers compete for the
  # same 10 slots, so competitor_mass spans both (computed before the type filter).
  field_prev   <- sum(base$prev_share, na.rm = TRUE)
  field_strong <- sum(base$prev_share >= 0.5, na.rm = TRUE)
  base <- base %>%
    dplyr::mutate(
      n_cand               = nrow(base),
      competitor_mass      = field_prev   - dplyr::coalesce(prev_share, 0),
      n_strong_competitors = field_strong - as.integer(dplyr::coalesce(prev_share, 0) >= 0.5)
    )

  base %>%
    dplyr::inner_join(merit, by = "playerID") %>%   # routes type + attaches merit
    .fill_ballot_na() %>%
    .strip_strength_if_off(cfg)
}

# ---- design / model ---------------------------------------------------------

#' Build the design matrix, keeping ALL rows (na.pass) so it stays aligned with
#' the response, then zero-fill any residual NAs (consistent with build_fit_matrix's
#' own zero-fill for absent seasons). Used identically for train and score.
.bbwaa_mm <- function(df) {
  mf <- stats::model.frame(.BBWAA_SHARE_FORMULA, data = df, na.action = stats::na.pass)
  x  <- stats::model.matrix(.BBWAA_SHARE_FORMULA, mf)[, -1, drop = FALSE]
  x[is.na(x)] <- 0
  x
}

.bbwaa_design <- function(df) list(x = .bbwaa_mm(df), y = df$share)

#' Reindex a design matrix onto the training feature columns (0 for absent ones,
#' e.g. a POS dummy or lag column missing from the score set).
.align_to <- function(X, feature_names) {
  out <- matrix(0, nrow = nrow(X), ncol = length(feature_names),
                dimnames = list(NULL, feature_names))
  common <- intersect(colnames(X), feature_names)
  if (length(common)) out[, common] <- X[, common, drop = FALSE]
  out
}

#' Train the vote-share regressor with grouped CV by ballot YEAR (leave-one-
#' cycle-out): a player's adjacent years are autocorrelated through prev_share,
#' so random k-fold would leak. OOF predictions are saved for honest evaluation
#' and for the residual pool the simulator resamples.
train_bbwaa_share <- function(train_df, seed, cv_folds = 10, engine = "xgbTree") {
  method <- .ENGINES[[engine]]
  if (is.null(method)) stop(sprintf("unknown engine '%s'", engine), call. = FALSE)
  train_df$POS <- as.factor(train_df$POS)
  d  <- .bbwaa_design(train_df)

  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  set.seed(seed)
  k     <- min(cv_folds, length(unique(train_df$yearID)))
  folds <- caret::groupKFold(train_df$yearID, k = k)
  ctrl  <- caret::trainControl(method = "cv", index = folds, savePredictions = "final")
  fit   <- caret::train(x = d$x, y = d$y, method = method, trControl = ctrl, verbose = FALSE)

  attr(fit, "feature_names") <- colnames(d$x)
  attr(fit, "pos_levels")    <- levels(train_df$POS)
  fit
}

#' Build the empirical-residual sampler the simulator draws its noise from.
#'
#' Residual variance is strongly heteroscedastic in predicted share: the model
#' nails low-share players (residual spike at ~0) but a candidate near the 75%
#' line has ~2x the spread. A single global pool therefore understates the upside
#' for boundary candidates (e.g. a 72% projection would get only a ~20% cycle
#' chance vs. a ~40% conditional/empirical rate). `conditional` mode bins the OOF
#' rows by predicted share and, at draw time, samples residuals from the ~k rows
#' whose fitted value is nearest the query share. `global` reproduces the old
#' single-pool behaviour (also the automatic fallback when data is too thin).
build_resid_sampler <- function(fit, mode = "conditional", nbins = 25, k = 150) {
  oof <- fit$pred
  if (is.null(oof) || !nrow(oof)) stop("fit has no OOF predictions", call. = FALSE)
  for (p in names(fit$bestTune)) oof <- oof[oof[[p]] == fit$bestTune[[p]], , drop = FALSE]
  pred <- oof$pred; resid <- oof$obs - oof$pred

  if (identical(mode, "global") || length(resid) < 2 * k) {
    return(list(mode = "global", pool = resid))
  }
  ord <- order(pred); pred <- pred[ord]; resid <- resid[ord]
  n <- length(resid); k <- min(k, n)
  centers <- seq(0, 1, length.out = nbins)
  pools <- matrix(0, nrow = nbins, ncol = k)
  for (b in seq_len(nbins)) {                     # each bin = the k nearest OOF rows by predicted share
    ip <- findInterval(centers[b], pred)
    lo <- max(1L, min(ip - k %/% 2L, n - k + 1L))
    pools[b, ] <- resid[lo:(lo + k - 1L)]
  }
  list(mode = "conditional", nbins = nbins, k = k, pools = pools, pool = resid)
}

#' Draw one residual per query predicted-share value `p` (vectorised).
draw_resid <- function(s, p) {
  if (identical(s$mode, "global")) return(sample(s$pool, length(p), replace = TRUE))
  bi  <- pmin(pmax(round(p * (s$nbins - 1)) + 1L, 1L), s$nbins)
  col <- sample.int(s$k, length(p), replace = TRUE)
  s$pools[cbind(bi, col)]
}

#' Held-out share metrics + how well predicted share separates the elected (>=75%).
evaluate_bbwaa <- function(fit) {
  oof <- fit$pred
  for (p in names(fit$bestTune)) oof <- oof[oof[[p]] == fit$bestTune[[p]], , drop = FALSE]
  obs <- oof$obs; pr <- oof$pred
  list(
    n             = length(obs),
    share_mae     = mean(abs(obs - pr)),
    share_rmse    = sqrt(mean((obs - pr)^2)),
    share_cor     = suppressWarnings(stats::cor(obs, pr)),
    elect_roc_auc = auc_roc(as.integer(obs >= 0.75), pr),
    elect_pr_auc  = auc_pr(as.integer(obs >= 0.75), pr),
    n_elect       = sum(obs >= 0.75),
    best_tune     = as.list(fit$bestTune)
  )
}

# ---- forward simulation -----------------------------------------------------

#' Monte-Carlo a single candidate's ballot future from one design row.
#'
#' Each year: predict share, add a resampled OOF residual, clip to [0,1]. Elect
#' if >= `elect`; drop off the ballot if < `survive`; otherwise advance (this
#' year's simulated share becomes next year's prev_share) until the era ballot
#' cap. Returns P(elected in year 1) and P(ever elected).
simulate_bbwaa <- function(fit, base_row, sampler, ballot_max,
                           n_sims = 2000, elect = 0.75, survive = 0.05,
                           ballot_decay = 0.85) {
  X <- matrix(base_row, nrow = n_sims, ncol = length(base_row), byrow = TRUE,
              dimnames = list(NULL, names(base_row)))
  prev  <- rep(base_row[["prev_share"]],      n_sims)
  prev2 <- rep(base_row[["prev2_share"]],     n_sims)
  maxp  <- rep(base_row[["max_share_prior"]], n_sims)
  app   <- base_row[["appearance_number"]]
  isf   <- base_row[["is_first_ballot"]]

  # Ballot strength (if used) decays over the sim: as the current logjam clears —
  # strong holdovers get elected/expire — competing demand on the ballot falls.
  has_str <- "competitor_mass"      %in% colnames(X)
  has_ns  <- "n_strong_competitors" %in% colnames(X)
  cmass   <- if (has_str) base_row[["competitor_mass"]]      else NA_real_
  cstrong <- if (has_ns)  base_row[["n_strong_competitors"]] else NA_real_

  alive <- rep(TRUE, n_sims); ever <- rep(FALSE, n_sims); yr1 <- rep(FALSE, n_sims)
  first_iter <- TRUE

  while (app <= ballot_max && any(alive)) {
    idx <- which(alive)
    X[idx, "appearance_number"] <- app
    X[idx, "is_first_ballot"]   <- isf
    X[idx, "years_remaining"]   <- ballot_max - app
    X[idx, "prev_share"]        <- prev[idx]
    X[idx, "prev2_share"]       <- prev2[idx]
    X[idx, "prev_delta"]        <- prev[idx] - prev2[idx]
    X[idx, "max_share_prior"]   <- maxp[idx]
    if (has_str) X[idx, "competitor_mass"]      <- cmass
    if (has_ns)  X[idx, "n_strong_competitors"] <- cstrong

    sh_hat <- as.numeric(stats::predict(fit, newdata = X[idx, , drop = FALSE]))
    sh     <- pmin(pmax(sh_hat + draw_resid(sampler, sh_hat), 0), 1)  # noise conditioned on this year's share

    elected_now <- sh >= elect
    dropped_now <- !elected_now & (sh < survive)
    ever[idx[elected_now]] <- TRUE
    if (first_iter) yr1[idx[elected_now]] <- TRUE

    prev2[idx] <- prev[idx]
    prev[idx]  <- sh
    maxp[idx]  <- pmax(maxp[idx], sh)
    alive[idx[elected_now | dropped_now]] <- FALSE

    app <- app + 1L
    isf <- 0
    cmass   <- cmass   * ballot_decay   # field thins as the backlog clears (n_cand held ~constant)
    cstrong <- cstrong * ballot_decay
    first_iter <- FALSE
  }
  list(p_cycle = mean(yr1), p_eventual = mean(ever))
}

#' Score the cycle's ballot: projected share + cycle/eventual election probs.
predict_bbwaa <- function(fit, score_df, sampler, cfg) {
  fn     <- attr(fit, "feature_names")
  pos_lv <- attr(fit, "pos_levels")

  sc <- score_df
  sc$POS    <- factor(as.character(sc$POS), levels = pos_lv)
  sc$share  <- 0; sc$elected <- FALSE; sc$yearID <- cfg$season   # phantom cols so `.` expands as in training
  X <- .align_to(.bbwaa_mm(sc), fn)

  pred_share <- pmin(pmax(as.numeric(stats::predict(fit, newdata = X)), 0), 1)

  ballot_max <- bbwaa_ballot_max_years(cfg$season)
  n_sims <- cfg$bbwaa$n_sims       %||% 2000
  elect  <- cfg$bbwaa$elect_pct    %||% 0.75
  surv   <- cfg$bbwaa$survive_pct  %||% 0.05
  decay  <- cfg$bbwaa$ballot_decay %||% 0.85

  sims <- lapply(seq_len(nrow(X)), function(i)
    simulate_bbwaa(fit, X[i, ], sampler, ballot_max,
                   n_sims = n_sims, elect = elect, survive = surv, ballot_decay = decay))

  # Merit stats carried through from the feature matrix for reporting/inspection.
  war_cols   <- grep("^WAR_[0-9]+$", names(score_df), value = TRUE)
  career_war <- if (length(war_cols)) rowSums(score_df[, war_cols, drop = FALSE], na.rm = TRUE) else NA_real_
  peak_war   <- if (length(war_cols)) do.call(pmax, c(as.list(score_df[, war_cols, drop = FALSE]), na.rm = TRUE)) else NA_real_
  seasons    <- if ("Seasons" %in% names(score_df)) score_df$Seasons else NA_real_
  getcol     <- function(nm) if (nm %in% names(score_df)) score_df[[nm]] else NA_real_

  ppl <- tbl("People") %>% dplyr::select(playerID, nameFirst, nameLast)
  out <- data.frame(
    playerID          = score_df$playerID,
    POS               = as.character(score_df$POS),
    appearance_number = score_df$appearance_number,
    is_first_ballot   = score_df$is_first_ballot,
    prev_share        = round(score_df$prev_share, 4),
    pred_share        = round(pred_share, 4),
    P_elected_cycle   = round(vapply(sims, `[[`, numeric(1), "p_cycle"), 4),
    P_eventual_bbwaa  = round(vapply(sims, `[[`, numeric(1), "p_eventual"), 4),
    Seasons           = seasons,
    CareerWAR         = round(career_war, 1),
    WARperSeason      = round(career_war / seasons, 2),
    PeakWAR           = round(peak_war, 1),
    JAWS              = round(getcol("jaws"), 1),
    HOFM              = round(getcol("HOFM_Points"), 0),
    TookSteroids      = as.integer(getcol("TookSteroids")),
    season            = cfg$season,
    stringsAsFactors  = FALSE
  )
  out %>%
    dplyr::left_join(ppl, by = "playerID") %>%
    dplyr::relocate(nameFirst, nameLast, .after = playerID)
}

# ---- persistence ------------------------------------------------------------

save_bbwaa_model <- function(fit, sampler, cfg, player_type, metrics) {
  dir <- file.path(cfg$paths$outputs, "models")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  base <- file.path(dir, sprintf("bbwaa_%s_%d", player_type, cfg$season))
  saveRDS(list(fit = fit, sampler = sampler,
               pos_levels = attr(fit, "pos_levels"),
               feature_names = attr(fit, "feature_names")),
          paste0(base, ".rds"))
  meta <- list(
    committee = "bbwaa", player_type = player_type, season = cfg$season,
    data_season = cfg$data_season, snapshot = cfg$snapshot,
    label_max_yr = cfg$dates$label_max_yr,
    engine = cfg$bbwaa$engine %||% "xgbTree",
    resid_mode = sampler$mode,
    n_sims = cfg$bbwaa$n_sims %||% 2000,
    n_features = length(attr(fit, "feature_names")),
    best_tune = as.list(fit$bestTune),
    metrics = metrics, trained_on = as.character(Sys.Date()),
    r_version = as.character(getRversion())
  )
  yaml::write_yaml(meta, paste0(base, ".meta.yml"))
  invisible(base)
}

save_bbwaa_eval <- function(metrics, cfg, player_type) {
  dir <- file.path(cfg$paths$outputs, "metrics")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  yaml::write_yaml(metrics, file.path(dir, sprintf("eval_bbwaa_%s_%d.yml", player_type, cfg$season)))
  invisible(dir)
}

# ---- orchestration (called from run_pipeline when model.committee == bbwaa) --

.run_bbwaa <- function(cfg, player_type = c("batter", "pitcher"),
                       war, cohorts, write = TRUE) {
  player_type <- match.arg(player_type)
  lmy    <- cfg$dates$label_max_yr

  panel  <- build_ballot_panel(label_max_year = lmy)
  merit  <- build_merit_matrix(war, player_type, cfg)
  train  <- build_bbwaa_train(panel, merit, cfg)

  engine <- cfg$bbwaa$engine %||% "xgbTree"
  seed   <- cfg$seeds[[player_type]]
  fit     <- train_bbwaa_share(train, seed = seed, cv_folds = cfg$model$cv_folds, engine = engine)
  sampler <- build_resid_sampler(fit, mode = cfg$bbwaa$resid_mode %||% "conditional")
  metrics <- evaluate_bbwaa(fit)

  # Projected prior cycles (model-predicted future ballots, from the carry-forward
  # driver) enter the SCORING state only — never training/the residual pool, which
  # would be circular. NULL on every normal run(.R) run, so panel_state == panel and
  # this path is byte-identical to a plain historical run. Known-ACTUAL results are
  # already folded into `panel` by build_ballot_panel(), so they are absent here.
  proj        <- cohorts$projected
  panel_state <- if (is.null(proj)) panel else dplyr::bind_rows(panel, proj)

  score  <- build_bbwaa_score(panel_state, merit, cohorts$ballot, cfg)
  res    <- predict_bbwaa(fit, score, sampler, cfg)
  res    <- res[order(-res$pred_share), , drop = FALSE]

  out <- file.path(cfg$paths$outputs, "predictions",
                   sprintf("hof_bbwaa_%ss_%d.csv", player_type, cfg$season))
  if (write) {
    if (!dir.exists(dirname(out))) dir.create(dirname(out), recursive = TRUE)
    utils::write.csv(res, out, row.names = FALSE)
    save_bbwaa_model(fit, sampler, cfg, player_type, metrics)
    save_bbwaa_eval(metrics, cfg, player_type)
  }
  list(fit = fit, results = res, out = out, metrics = metrics,
       train = train, score = score, committee = "bbwaa")
}
