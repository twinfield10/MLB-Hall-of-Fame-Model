# =============================================================================
# Career projection — Monte-Carlo HOF forecasting for active players.
#
# Stage 2 of active-player forecasting. Frozen scoring (R/active_players.R) judges
# a mid-career résumé as if it were complete; here we PROJECT the rest of the
# career and score the projected FINAL résumé, integrating over uncertainty.
#
# Method (per the user's choice: Marcel-style aging curves, full stat line,
# Monte-Carlo):
#   * Baseline   — Marcel 5/4/3 weighting of the last 3 real seasons (rate stats
#                  per PA/IP, WAR, playing time, WAR components).
#   * Aging      — an empirical age multiplier applied year-over-year to WAR and
#                  playing time until the player "retires" (production/PT below a
#                  floor or an age cap).
#   * Monte-Carlo— each of K sims perturbs season WAR (log-normal), playing time,
#                  and career length (an age-rising retirement hazard), producing
#                  a DISTRIBUTION of projected final résumés.
#   * Score      — each sim's résumé is reshaped through the real feature pipeline
#                  (get_fit_data is per-player — validated) and scored by the
#                  any-route merit model. P(HOF) = mean over sims; we also report
#                  the projected-WAR distribution.
#
# Approximations (documented): future seasons have no league to rank within or
# win awards from, so WAA_rank / OPS+ / ERA+ are carried from the recent baseline,
# future award & all-star flags are 0, and per-season HOF-Monitor points are
# carried (age-declined) rather than recomputed with league-leader bonuses. WAR,
# JAWS, playing time and milestone counting stats — the dominant signals — are
# projected explicitly. jaws/d_jaws are recomputed over the augmented career.
# =============================================================================

suppressMessages({ library(dplyr) })

if (!exists("build_split_batters")) source(file.path("R", "pipeline.R"))
if (!exists("train_hof_model"))     source(file.path("R", "model.R"))
if (!exists("active_player_ids"))   source(file.path("R", "active_players.R"))

`%||%`   <- function(a, b) if (is.null(a) || length(a) == 0) b else a
`%notin%` <- Negate(`%in%`)

# ---- aging curves (per-year retention factors) ------------------------------
# Multiplicative WAR retention vs. the prior projected season; PT decays more
# gently. Batters age a touch better than pitchers.
.age_mult <- function(age, type) {
  if (type == "batter") dplyr::case_when(age<=27~1.03, age<=29~1.00, age<=31~0.95,
                                         age<=33~0.90, age<=35~0.85, age<=37~0.80, TRUE~0.72)
  else                  dplyr::case_when(age<=26~1.02, age<=29~0.98, age<=31~0.93,
                                         age<=33~0.88, age<=35~0.82, age<=37~0.76, TRUE~0.68)
}
.pt_mult <- function(age, type) {
  if (type == "batter") dplyr::case_when(age<=30~1.00, age<=33~0.95, age<=36~0.88, TRUE~0.80)
  else                  dplyr::case_when(age<=30~1.00, age<=33~0.93, age<=36~0.85, TRUE~0.75)
}
# Annual retirement hazard (probability the career ends despite being above the
# production floor) — the Monte-Carlo source of career-length uncertainty. Rises
# with age but is LOW for productive seasons (stars play through their 30s) and
# climbs as projected WAR fades toward replacement (injury/benching/choice).
.retire_hazard <- function(age, wr) pmin(0.85, pmax(0.02, 0.04 * (age - 32) + 0.15 * pmax(0, 2.5 - wr)))

# per-type spec: rate columns to carry per playing-time unit, PT column, floors
.type_spec <- function(type) {
  if (type == "batter") list(
    pt = "PA", floor_pt = 200, floor_war = 0.5, age_cap = 43,
    rates = c("H","HR","RBI","R","BB","SO","SB","AB","X2B","X3B","TB","HBP","SF","SH","GIDP","IBB","G","CS"),
    zero  = c("MVPWin","ROYWin","SS_Win","GG_Win","TC_Win","WSWin","PenWin","DivWin","AllStar","AllStarStart","MVPShare"),
    carry = c("OPS_plus"), war_parts = c(WAR_off="WAR_off", WAR_def="WAR_def"))
  else list(
    pt = "IPouts", floor_pt = 120, floor_war = 0.5, age_cap = 44,
    rates = c("W","L","SV","SO","BB","H","ER","HR","R","G","GS","BAOpp_num","IBB","HBP","WP","BK","TBF"),
    zero  = c("MVPWin","ROYWin","GG_Win","CYWin","AllStar","AllStarStart","MVPShare","CYShare"),
    carry = c("ERA_plus","BAOpp"), war_parts = c())
}

# ---- Marcel baseline --------------------------------------------------------

#' Weighted-recent baseline for a player's per-season production & rates.
.marcel_baseline <- function(real, type) {
  spec <- .type_spec(type)
  rec  <- utils::tail(real[order(real$yearID), ], 3)
  w    <- c(3, 4, 5)[(3 - nrow(rec) + 1):3]; wn <- w / sum(w)
  pt   <- real[[spec$pt]]; recpt <- rec[[spec$pt]]
  bPT  <- sum(wn * recpt); bWAR <- sum(wn * rec$WAR)
  rates <- vapply(spec$rates, function(c)
    if (c %in% names(rec)) sum(wn * rec[[c]]) / max(bPT, 1) else 0, numeric(1))
  carry <- vapply(spec$carry, function(c)
    if (c %in% names(rec)) sum(wn * rec[[c]]) else 0, numeric(1))
  list(
    bPT = bPT, bWAR = bWAR, rates = rates, carry = carry,
    waaR = sum(wn * rec$WAA) / max(bWAR, 1e-6),
    offS = if (length(spec$war_parts)) mean(rec$WAR_off / pmax(rec$WAR, 1e-6)) else 0,
    defS = if (length(spec$war_parts)) mean(rec$WAR_def / pmax(rec$WAR, 1e-6)) else 0,
    bRank = sum(wn * rec$WAA_rank), bHOFM = sum(wn * rec$HOFM_Points),
    warSD = max(stats::sd(rec$WAR), 0.5)
  )
}

# ---- one projected career (deterministic median, or a Monte-Carlo draw) -----

#' Append a projected remaining career to a player's real long rows. When
#' `rng` is NULL the median path is generated; otherwise season WAR, playing time
#' and career length are perturbed for one Monte-Carlo draw.
project_career <- function(real, type, rng = NULL) {
  spec <- .type_spec(type); b <- .marcel_baseline(real, type)
  cur_age <- max(real$Age); tmpl <- real[nrow(real), ]; lastyr <- max(real$yearID)
  proj <- list(); pt <- b$bPT; wr <- b$bWAR
  for (y in 1:22) {
    age <- cur_age + y
    wr  <- wr * .age_mult(age, type); pt <- pt * .pt_mult(age, type)
    if (!is.null(rng)) wr <- wr * exp(stats::rnorm(1, 0, b$warSD / max(abs(b$bWAR), 2)))  # log-normal season noise
    if (wr < spec$floor_war || pt < spec$floor_pt || age > spec$age_cap) break            # decline-driven end
    if (!is.null(rng) && stats::runif(1) < .retire_hazard(age, wr)) break                 # injury/choice end (perf-aware)
    r <- tmpl; r$yearID <- lastyr + y; r$Age <- age; r[[spec$pt]] <- pt
    for (c in names(b$rates)) if (c %in% names(r)) r[[c]] <- b$rates[[c]] * pt
    r$WAR <- wr; r$WAA <- wr * b$waaR
    if (length(spec$war_parts)) { r$WAR_off <- wr * b$offS; r$WAR_def <- wr * b$defS }
    for (c in names(b$carry)) if (c %in% names(r)) r[[c]] <- b$carry[[c]]
    if ("WAA_rank" %in% names(r))    r$WAA_rank    <- b$bRank
    if ("HOFM_Points" %in% names(r)) r$HOFM_Points <- b$bHOFM * .age_mult(age, type)
    for (c in spec$zero) if (c %in% names(r)) r[[c]] <- 0
    r$inducted <- "N"
    proj[[length(proj) + 1]] <- r
  }
  proj <- if (length(proj)) do.call(rbind, proj) else real[0, ]
  comb <- rbind(real, proj)
  wsort <- sort(comb$WAR, decreasing = TRUE)
  comb$jaws <- 0.5 * (sum(comb$WAR) + sum(utils::head(wsort, 7)))
  if ("d_jaws" %in% names(comb) && length(spec$war_parts)) {
    dsort <- sort(comb$WAR_def, decreasing = TRUE)
    comb$d_jaws <- 0.5 * (sum(comb$WAR_def) + sum(utils::head(dsort, 7)))
  }
  comb$finalGame <- as.Date(sprintf("%d-10-01", max(comb$yearID)))
  comb
}

# ---- scoring ----------------------------------------------------------------

#' P(HOF) from the any-route merit model, robust for one player or a batch:
#' drop playerID/inducted (single-value factors break model.matrix even under
#' `- playerID`), pin POS to the training levels, then align to the fit's columns.
.merit_predict <- function(fit, df, pos_levels) {
  fn <- attr(fit, "feature_names")
  df$POS <- factor(as.character(df$POS), levels = pos_levels)
  df$playerID <- NULL; df$inducted <- NULL
  X  <- stats::model.matrix(~ ., data = df)[, -1, drop = FALSE]
  Xa <- matrix(0, nrow(X), length(fn), dimnames = list(NULL, fn))
  cm <- intersect(colnames(X), fn); if (length(cm)) Xa[, cm] <- X[, cm]
  as.numeric(stats::predict(fit, newdata = Xa, type = "prob")[, "Y"])
}

#' Reshape a batch of (real+projected) long rows through the real feature pipeline
#' and score. Returns a named vector P(HOF) keyed by playerID, plus career WAR/JAWS.
.score_batch <- function(long_batch, type, fit, pos_levels) {
  drop <- if (type == "batter") .BAT_DROP else .PIT_DROP
  agg  <- if (type == "batter") get_aggregated_fit_df else p_get_aggregated_fit_df
  fitd <- if (type == "batter") get_fit_data else p_get_fit_data
  m <- fitd(long_batch, final_game_min = "1901-01-01", final_game_max = "2100-12-31")
  m <- m %>% dplyr::inner_join(agg(m), by = "playerID") %>% dplyr::select(-dplyr::starts_with(drop))
  wc <- grep("^WAR_[0-9]+$", names(m), value = TRUE)
  list(P = setNames(.merit_predict(fit, m, pos_levels), m$playerID),
       WAR = setNames(rowSums(m[, wc, drop = FALSE], na.rm = TRUE), m$playerID),
       JAWS = setNames(m$jaws, m$playerID))
}

# ---- training + pool --------------------------------------------------------

#' Train the any-route merit models (batter & pitcher) and capture POS levels &
#' the assembled real long frames (reused for every projection).
train_merit_fits <- function(cfg, war, cohorts) {
  out <- list()
  for (type in c("batter", "pitcher")) {
    long  <- if (type == "batter") assemble_batters(war,  label_max_year = cfg$dates$label_max_yr)
             else                  assemble_pitchers(war, label_max_year = cfg$dates$label_max_yr)
    split <- if (type == "batter") build_split_batters(long, cohorts, cfg)
             else                  build_split_pitchers(long, cohorts, cfg)
    fit   <- train_hof_model(split$train, seed = cfg$seeds[[type]], cv_folds = cfg$model$cv_folds,
                             engine = cfg$model$engine, imbalance = cfg$model$imbalance)
    out[[type]] <- list(fit = fit, pos_levels = sort(unique(as.character(split$train$POS))), long = long)
  }
  out
}

#' Active projection pool: for each active player with >= min_seasons and a
#' plausible résumé, their type and real long rows. Young stars qualify via recent
#' form even with modest career totals.
build_projection_pool <- function(cfg, fits, min_seasons = 4, min_career_war = 12) {
  ids <- active_player_ids(cfg)
  pool <- list()
  for (type in c("batter", "pitcher")) {
    long <- fits[[type]]$long
    pos_bat <- long$POS %notin% c("SP", "RP")
    sel <- if (type == "batter") long$playerID[pos_bat] else long$playerID[!pos_bat]
    cand <- intersect(ids, unique(sel))
    for (pid in cand) {
      real <- long[long$playerID == pid, , drop = FALSE]
      real <- real[!is.na(real$WAR), , drop = FALSE]
      if (nrow(real) < min_seasons) next
      cw <- sum(real$WAR, na.rm = TRUE)
      recent_war <- mean(utils::tail(real[order(real$yearID), ]$WAR, 3), na.rm = TRUE)
      if (cw < min_career_war && recent_war < 1.5) next     # skip clear non-candidates
      pool[[pid]] <- list(type = type, real = real)
    }
  }
  pool
}

# ---- Monte-Carlo forecast ---------------------------------------------------

#' Monte-Carlo HOF forecast for the active projection pool. For each player, K
#' sims draw a projected career (aging + WAR/PT noise + stochastic length); each
#' sim's projected résumé is scored by the merit model. Returns a per-player
#' summary: frozen P(HOF), MC mean P(HOF) with p10-p90 range, and projected-WAR /
#' added-years distribution. Sims are batched per draw for speed.
project_active_hof <- function(cfg, war, cohorts = NULL, K = 300,
                               min_seasons = 4, min_career_war = 12, seed = 8642) {
  if (is.null(cohorts)) cohorts <- build_cohorts(cfg)
  fits <- train_merit_fits(cfg, war, cohorts)
  pool <- build_projection_pool(cfg, fits, min_seasons, min_career_war)
  ids  <- names(pool)
  types <- vapply(pool, `[[`, character(1), "type")

  set.seed(seed)
  # frozen + deterministic-median projection (one batch per type)
  froz <- list(); medp <- list(); med_war <- list(); med_add <- list()
  for (type in c("batter", "pitcher")) {
    tids <- ids[types == type]; if (!length(tids)) next
    f <- fits[[type]]
    froz_long <- do.call(rbind, lapply(tids, function(p) pool[[p]]$real))
    fb <- .score_batch(froz_long, type, f$fit, f$pos_levels)
    med_long <- do.call(rbind, lapply(tids, function(p) project_career(pool[[p]]$real, type, rng = NULL)))
    mb <- .score_batch(med_long, type, f$fit, f$pos_levels)
    for (p in tids) {
      froz[[p]] <- fb$P[[p]]; medp[[p]] <- mb$P[[p]]; med_war[[p]] <- mb$WAR[[p]]
      med_add[[p]] <- max(0L, length(unique(project_career(pool[[p]]$real, type)$yearID)) - nrow(pool[[p]]$real))
    }
  }

  # Monte-Carlo: K draws, batched per type per draw
  sims_P   <- setNames(vector("list", length(ids)), ids)
  sims_WAR <- setNames(vector("list", length(ids)), ids)
  for (k in seq_len(K)) {
    for (type in c("batter", "pitcher")) {
      tids <- ids[types == type]; if (!length(tids)) next
      f <- fits[[type]]
      draw_long <- do.call(rbind, lapply(tids, function(p) project_career(pool[[p]]$real, type, rng = TRUE)))
      sb <- .score_batch(draw_long, type, f$fit, f$pos_levels)
      for (p in tids) { sims_P[[p]] <- c(sims_P[[p]], sb$P[[p]]); sims_WAR[[p]] <- c(sims_WAR[[p]], sb$WAR[[p]]) }
    }
  }

  np <- tbl("People")
  qs <- function(x, q) as.numeric(stats::quantile(x, q, names = FALSE, na.rm = TRUE))
  dplyr::bind_rows(lapply(ids, function(p) {
    who <- np$playerID == p
    dplyr::tibble(
      playerID = p, name = trimws(paste(np$nameFirst[who], np$nameLast[who])),
      type = ifelse(types[[p]] == "batter", "BAT", "PIT"),
      Age = cfg$dates$age_as_of - np$birthYear[who],
      real_seasons = nrow(pool[[p]]$real),
      real_WAR = round(sum(pool[[p]]$real$WAR, na.rm = TRUE), 1),
      P_HOF_frozen = round(froz[[p]] %||% NA_real_, 3),
      P_HOF        = round(mean(sims_P[[p]]), 3),
      P_HOF_p10    = round(qs(sims_P[[p]], 0.10), 3),
      P_HOF_p90    = round(qs(sims_P[[p]], 0.90), 3),
      proj_WAR_med = round(qs(sims_WAR[[p]], 0.50), 1),
      proj_WAR_p10 = round(qs(sims_WAR[[p]], 0.10), 1),
      proj_WAR_p90 = round(qs(sims_WAR[[p]], 0.90), 1),
      added_years  = med_add[[p]] %||% NA_integer_
    )
  })) %>% dplyr::arrange(dplyr::desc(P_HOF), dplyr::desc(proj_WAR_med))
}

