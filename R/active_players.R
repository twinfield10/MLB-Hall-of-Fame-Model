# =============================================================================
# Active-player scoring — "where are they, career frozen at the data season?"
#
# The FIRST step toward active-player HOF forecasting: no career projection yet.
# Take every player still active in the data season, freeze their careers as-is,
# and score them two ways:
#
#   (1) Merit P(HOF)     — the any-route `inducted ~ merit` model (career résumé
#                          -> probability of eventual induction by anyone). This
#                          is the natural "where are they" signal: active players
#                          have no ballot history, so the vote-share model can't
#                          score them directly.
#   (2) BBWAA if-retired — score them through the committee vote model AS IF they
#                          retired today and debuted on a TYPICAL modern ballot
#                          (first-ballot state, neutral field): projected debut
#                          share + P(elected this cycle) + P(ever elected).
#
# CAVEAT (by design): both models were trained on COMPLETED careers, so a
# mid-career star (frozen) is judged on totals they haven't finished compiling —
# they will look under-qualified until the projection phase (Stage 2) extends
# their careers. That gap is the whole point of scoring frozen first.
# =============================================================================

suppressMessages({ library(dplyr) })

if (!exists("build_ballot_panel"))  source(file.path("R", "ballot_features.R"))
if (!exists("build_split_batters")) source(file.path("R", "pipeline.R"))
if (!exists("train_hof_model"))     source(file.path("R", "model.R"))

`%||%`   <- function(a, b) if (is.null(a) || length(a) == 0) b else a
`%notin%` <- Negate(`%in%`)

#' Players still active in the data season = anyone with a Batting/Pitching row
#' in `cfg$data_season`. (Active players carry finalGame = NA in the People
#' snapshot; the playing-time tables are the reliable "still active" signal.)
active_player_ids <- function(cfg) {
  ds <- cfg$data_season
  unique(c(tbl("Batting")$playerID[tbl("Batting")$yearID == ds],
           tbl("Pitching")$playerID[tbl("Pitching")$yearID == ds]))
}

# ---- (1) merit P(HOF) via the any-route model -------------------------------

#' Predict P(HOF) from a fitted any-route model, aligning the score matrix to the
#' training feature columns (active players' POS mix differs, so raw model.matrix
#' columns won't match — mirror committee_bbwaa.R::.align_to).
.merit_predict <- function(fit, df) {
  fn <- attr(fit, "feature_names")
  X  <- stats::model.matrix(stats::as.formula("inducted ~ . - playerID"), data = df)[, -1, drop = FALSE]
  Xa <- matrix(0, nrow(X), length(fn), dimnames = list(NULL, fn))
  common <- intersect(colnames(X), fn)
  if (length(common)) Xa[, common] <- X[, common]
  as.numeric(stats::predict(fit, newdata = Xa, type = "prob")[, "Y"])
}

#' Career-merit feature matrix for the ACTIVE cohort of one player type, built
#' exactly like the any-route train/test frames (same feature functions), over
#' the data-season final-game window so active players are captured.
.active_merit_frame <- function(war, type, cfg) {
  lmy <- cfg$dates$label_max_yr
  ds  <- cfg$data_season
  win_lo <- sprintf("%d-01-01", ds); win_hi <- sprintf("%d-12-31", ds)
  if (type == "batter") {
    tmp <- assemble_batters(war, label_max_year = lmy) %>% dplyr::filter(POS %notin% c("SP", "RP"))
    m <- get_fit_data(tmp, final_game_min = win_lo, final_game_max = win_hi) %>%
      dplyr::filter(PA_1 >= cfg$thresholds$min_pa)
    m %>% dplyr::inner_join(get_aggregated_fit_df(m), by = "playerID") %>%
      dplyr::select(-dplyr::starts_with(.BAT_DROP))
  } else {
    tmp <- assemble_pitchers(war, label_max_year = lmy) %>% dplyr::filter(POS %in% c("SP", "RP"))
    m <- p_get_fit_data(tmp, final_game_min = win_lo, final_game_max = win_hi) %>%
      dplyr::filter(IPouts_1 >= cfg$thresholds$min_ipouts)
    m %>% dplyr::inner_join(p_get_aggregated_fit_df(m), by = "playerID") %>%
      dplyr::select(-dplyr::starts_with(.PIT_DROP))
  }
}

#' Score active players' frozen résumés with the any-route merit model.
#' Returns tibble(playerID, type, Seasons, CareerWAR, WARperSeason, PeakWAR,
#' JAWS, HOFM, P_HOF).
score_active_merit <- function(cfg, war, cohorts) {
  one <- function(type) {
    if (type == "batter") {
      split <- build_split_batters(assemble_batters(war, label_max_year = cfg$dates$label_max_yr), cohorts, cfg)
    } else {
      split <- build_split_pitchers(assemble_pitchers(war, label_max_year = cfg$dates$label_max_yr), cohorts, cfg)
    }
    fit <- train_hof_model(split$train, seed = cfg$seeds[[type]], cv_folds = cfg$model$cv_folds,
                           engine = cfg$model$engine, imbalance = cfg$model$imbalance)
    act <- .active_merit_frame(war, type, cfg)
    wc  <- grep("^WAR_[0-9]+$", names(act), value = TRUE)
    cw  <- rowSums(act[, wc, drop = FALSE], na.rm = TRUE)
    dplyr::tibble(
      playerID     = act$playerID,
      type         = ifelse(type == "batter", "BAT", "PIT"),
      Seasons      = act$Seasons,
      CareerWAR    = round(cw, 1),
      WARperSeason = round(cw / act$Seasons, 2),
      PeakWAR      = round(do.call(pmax, c(as.list(act[, wc, drop = FALSE]), na.rm = TRUE)), 1),
      JAWS         = round(act$jaws, 1),
      HOFM         = round(act$HOFM_Points),
      P_HOF        = round(.merit_predict(fit, act), 3)
    )
  }
  dplyr::bind_rows(one("batter"), one("pitcher"))
}

# ---- (2) BBWAA "if retired today" via the committee vote model --------------

#' Typical modern-ballot field a newcomer debuts against (mean over 2015+ real
#' cycles): a first-timer's competitor_mass = sum of the OTHER candidates' prior
#' shares, so ~ the ballot's total prev_share. Used so active players aren't scored
#' against an empty field (which would inflate every debut share).
.neutral_field <- function(panel) {
  per_yr <- panel %>%
    dplyr::filter(yearID >= 2015) %>%
    dplyr::group_by(yearID) %>%
    dplyr::summarise(n_cand = dplyr::n(),
                     mass   = sum(prev_share, na.rm = TRUE),
                     strong = sum(prev_share >= 0.5, na.rm = TRUE), .groups = "drop")
  list(competitor_mass = mean(per_yr$mass),
       n_cand          = mean(per_yr$n_cand),
       n_strong        = mean(per_yr$strong))
}

#' Score active players through the committee model as hypothetical first-ballot
#' candidates on a typical modern field. Returns predict_bbwaa output columns
#' (pred_share, P_elected_cycle, P_eventual_bbwaa, merit stats) for active players.
score_active_bbwaa <- function(cfg, war, active_ids) {
  panel <- build_ballot_panel(label_max_year = cfg$dates$label_max_yr)
  nf    <- .neutral_field(panel)
  one <- function(type) {
    merit   <- build_merit_matrix(war, type, cfg)
    train   <- build_bbwaa_train(panel, merit, cfg)
    fit     <- train_bbwaa_share(train, seed = cfg$seeds[[type]], cv_folds = cfg$model$cv_folds,
                                 engine = cfg$bbwaa$engine %||% "xgbTree")
    sampler <- build_resid_sampler(fit, mode = cfg$bbwaa$resid_mode %||% "conditional")
    ids     <- intersect(active_ids, merit$playerID)     # active players are never in the panel -> first-timers
    if (!length(ids)) return(NULL)
    score   <- build_bbwaa_score(panel, merit, ids, cfg)
    # replace the empty-field crowding (all first-timers -> mass 0, n_cand huge)
    # with a typical modern ballot, so debut shares are realistic.
    if (isTRUE(cfg$bbwaa$use_ballot_strength %||% TRUE)) {
      score$competitor_mass      <- nf$competitor_mass
      score$n_cand               <- nf$n_cand
      score$n_strong_competitors <- nf$n_strong
    }
    predict_bbwaa(fit, score, sampler, cfg)
  }
  dplyr::bind_rows(Filter(Negate(is.null), list(one("batter"), one("pitcher"))))
}

# ---- combined report --------------------------------------------------------

#' Full frozen-career report for active players (>= min_seasons). Merges the merit
#' P(HOF) and BBWAA if-retired views, adds age & name, sorts by P(HOF).
build_active_report <- function(cfg, war, cohorts = NULL, min_seasons = 10) {
  if (is.null(cohorts)) cohorts <- build_cohorts(cfg)
  ids   <- active_player_ids(cfg)

  merit <- score_active_merit(cfg, war, cohorts)
  bbwaa <- score_active_bbwaa(cfg, war, ids) %>%
    dplyr::transmute(playerID,
                     proj_share_ifretired = pred_share,
                     P_elected_ifretired  = P_elected_cycle,
                     P_eventual_ifretired = P_eventual_bbwaa)

  ppl <- tbl("People") %>%
    dplyr::transmute(playerID, nameFirst, nameLast,
                     Age = cfg$dates$age_as_of - birthYear)

  merit %>%
    dplyr::left_join(bbwaa, by = "playerID") %>%
    dplyr::left_join(ppl,   by = "playerID") %>%
    dplyr::relocate(nameFirst, nameLast, Age, .after = playerID) %>%
    dplyr::filter(Seasons >= min_seasons) %>%
    dplyr::arrange(dplyr::desc(P_HOF), dplyr::desc(CareerWAR))
}
