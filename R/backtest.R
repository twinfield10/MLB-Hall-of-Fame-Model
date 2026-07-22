# =============================================================================
# Walk-forward historical backtest of the BBWAA committee model.
#
# For each past cycle Y, train the vote-share model ONLY on ballot rows with
# yearID < Y (no leakage — the model never sees year Y or later), then score the
# actual year-Y ballot and pair each prediction with the real outcome. This is
# the honest "how would the model have called past ballots" test that feeds the
# frontend's Historical tab.
#
# The full panel and per-type merit matrix are built ONCE by the driver
# (Scripts/backtest_ballots.R) and reused across years — merit is career-static
# and the panel already carries one row per real ballot appearance, so
# panel[yearID == Y] IS the actual year-Y ballot and panel[yearID < Y] is the
# leak-free training set.
# =============================================================================

suppressMessages({ library(dplyr) })

if (!exists("build_ballot_panel")) source(file.path("R", "ballot_features.R"))
if (!exists("train_bbwaa_share"))  source(file.path("R", "committee_bbwaa.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Actual BBWAA outcomes per (player, year): votes, ballots, share, inducted.
#' Combines the snapshot HallOfFame with any Data/ballots/results_<Y>.csv (e.g.
#' the real 2026 vote, which post-dates the Lahman release).
build_actuals <- function() {
  norm <- function(df) df %>%
    dplyr::filter(votedBy == "BBWAA", category == "Player",
                  !is.na(votes), !is.na(ballots), ballots > 0) %>%
    dplyr::transmute(playerID, yearID,
                     actual_votes = votes, actual_ballots = ballots,
                     actual_share = votes / ballots,
                     actual_inducted = !is.na(inducted) & inducted == "Y")
  hof   <- norm(tbl("HallOfFame"))
  known <- read_known_results()
  if (!is.null(known) && nrow(known)) {
    if (!"category" %in% names(known)) known$category <- "Player"
    hof <- dplyr::bind_rows(hof, norm(known)) %>%
      dplyr::distinct(playerID, yearID, .keep_all = TRUE)
  }
  hof
}

#' Backtest a single cycle Y for one player type. Returns the predict_bbwaa()
#' frame (no actuals joined yet), or NULL if no candidates of this type.
backtest_one <- function(Y, cfg, panel, merit, player_type, n_sims = NULL) {
  cfg_y <- cfg
  cfg_y$season <- as.integer(Y)                       # scoring fns read only $season/$bbwaa
  if (!is.null(n_sims)) cfg_y$bbwaa$n_sims <- as.integer(n_sims)

  train <- build_bbwaa_train(dplyr::filter(panel, yearID < Y), merit, cfg_y)
  if (!nrow(train)) return(NULL)
  fit <- train_bbwaa_share(train, seed = cfg_y$seeds[[player_type]],
                           cv_folds = cfg_y$model$cv_folds,
                           engine   = cfg_y$bbwaa$engine %||% "xgbTree")
  sampler <- build_resid_sampler(fit, mode = cfg_y$bbwaa$resid_mode %||% "conditional")

  cohort_ids <- unique(panel$playerID[panel$yearID == Y])
  score <- build_bbwaa_score(panel, merit, cohort_ids, cfg_y)
  if (!nrow(score)) return(NULL)
  predict_bbwaa(fit, score, sampler, cfg_y)
}

#' Full backtest of cycle Y (batters + pitchers), with actuals joined and the
#' model's elect/no-elect call scored. Returns one combined data.frame.
backtest_cycle <- function(Y, cfg, panel, actuals, merit_b, merit_p, n_sims = NULL) {
  res <- dplyr::bind_rows(
    backtest_one(Y, cfg, panel, merit_b, "batter",  n_sims = n_sims),
    backtest_one(Y, cfg, panel, merit_p, "pitcher", n_sims = n_sims)
  )
  if (is.null(res) || !nrow(res)) return(NULL)
  elect_pct <- cfg$bbwaa$elect_pct %||% 0.75
  res %>%
    dplyr::left_join(dplyr::select(dplyr::filter(actuals, yearID == Y), -yearID),
                     by = "playerID") %>%
    dplyr::mutate(
      actual_inducted = dplyr::coalesce(actual_inducted, FALSE),
      pred_call       = pred_share >= elect_pct,
      pred_hit        = pred_call == actual_inducted
    ) %>%
    dplyr::arrange(dplyr::desc(pred_share), dplyr::desc(P_eventual_bbwaa))
}
