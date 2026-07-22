# =============================================================================
# Ballot-history features (BBWAA committee model).
#
# The any-route model uses only career merit and never looks at how a candidate
# has actually fared on the writers' ballot. For the BBWAA vote, the single
# strongest signal is a candidate's *prior-year vote share* and their trajectory
# (rising vs. fading). This module reconstructs the player-ballot-year panel from
# Lahman's HallOfFame table and derives those dynamics.
#
# Vote share = votes / ballots (the HallOfFame `needed` column is ~0.75*ballots,
# confirming the 75% election rule). All ballot-history features for a given
# (player, year) row are computed STRICTLY from years < that row's year, so the
# target (share/elected in that year) never leaks into its own predictors.
# =============================================================================

suppressMessages({ library(dplyr) })

if (!exists("tbl"))          source(file.path("R", "tables.R"))
if (!exists("get_fit_data")) source(file.path("R", "features.R"))
if (!exists("assemble_batters")) source(file.path("R", "assemble.R"))

`%notin%` <- Negate(`%in%`)

# Aggregate award/season columns dropped after the career-aggregate join. These
# MUST match .BAT_DROP / .PIT_DROP in R/pipeline.R so the BBWAA model sees the
# exact same merit feature set as the any-route model.
.BBWAA_BAT_DROP <- c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "ROYWin_", "TC_Win_",
                     "AllStar_", "C_HOFM_Points")
.BBWAA_PIT_DROP <- c("MVPWin_", "GG_Win_", "CYWin_", "AllStar_", "ROYWin_", "C_HOFM_Points")

#' Maximum number of years a candidate may remain on the BBWAA ballot, by era.
#' 10-year rule effective for the 2015 cycle (adopted July 2014); 15-year rule
#' before that; wider/undefined earlier (approximate — only feeds `years_remaining`).
bbwaa_ballot_max_years <- function(year) {
  dplyr::case_when(
    year >= 2015 ~ 10L,
    year >= 1963 ~ 15L,
    TRUE         ~ 20L
  )
}

#' Read known-actual BBWAA ballot results (Data/ballots/results_<year>.csv) for
#' cycles the snapshot's HallOfFame table does not yet carry (e.g. the real 2026
#' vote, announced after the 2025 Lahman release). These are REAL results, used
#' exactly like snapshot rows — they enter both training and holdover derivation.
#' Schema mirrors HallOfFame (playerID, yearID, votedBy, ballots, needed, votes,
#' inducted, category); an optional playerName column is present for review and
#' ignored here. Returns NULL when no such files exist.
read_known_results <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    cfg <- tryCatch(active_cfg(), error = function(e) NULL)
    data_dir <- if (!is.null(cfg)) cfg$paths$data else "Data"
  }
  dir   <- file.path(data_dir, "ballots")
  files <- list.files(dir, pattern = "^results_[0-9]{4}\\.csv$", full.names = TRUE)
  if (!length(files)) return(NULL)
  cols <- c("playerID", "yearID", "votedBy", "ballots", "needed", "votes",
            "inducted", "category")
  rows <- lapply(files, function(f) {
    d <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
    d[, intersect(cols, names(d)), drop = FALSE]
  })
  dplyr::bind_rows(rows)
}

#' Build the player-ballot-year panel from the HallOfFame table.
#'
#' One row per actual BBWAA ballot appearance (1936..label_max_year). Because we
#' train on real ballot rows, BBWAA eligibility (10 seasons played, retired 5 yrs,
#' 5% survival, <=10-year tenure, nomination) is self-enforcing — a player only has
#' rows here if they cleared those bars.
#'
#' Known-actual results (read_known_results()) for cycles AFTER the snapshot's last
#' real BBWAA year are unioned in before the label filter, so a refreshed real
#' result (e.g. the 2026 vote) grounds later predictions. The `yearID > snap_max`
#' guard prevents any double-count with the snapshot, and the label_max_year filter
#' still applies — so a results_<Y>.csv is inert for any run with label_max_yr < Y
#' (e.g. adding results_2026.csv does NOT change the season-2026 run: label<=2025).
#'
#' @param label_max_year cap ballot years to avoid peeking at the season being
#'   predicted (the 2025 snapshot's HallOfFame already carries the 2026 class).
#' @return tibble: playerID, yearID, share, elected, appearance_number,
#'   is_first_ballot, prev_share, prev2_share, prev_delta, max_share_prior,
#'   years_remaining. Lagged features are NA on a player's first appearance.
build_ballot_panel <- function(label_max_year = NULL) {
  hof   <- tbl("HallOfFame")
  known <- read_known_results()
  if (!is.null(known) && nrow(known)) {
    snap_max <- suppressWarnings(max(
      hof$yearID[hof$votedBy == "BBWAA" & hof$category == "Player"], na.rm = TRUE))
    known <- dplyr::filter(known, yearID > snap_max)
    if (nrow(known)) hof <- dplyr::bind_rows(hof, known)
  }
  if (!is.null(label_max_year)) hof <- dplyr::filter(hof, yearID <= label_max_year)

  hof %>%
    dplyr::filter(votedBy == "BBWAA", category == "Player",
                  !is.na(votes), !is.na(ballots), ballots > 0) %>%
    dplyr::transmute(
      playerID, yearID,
      share   = votes / ballots,
      elected = !is.na(inducted) & inducted == "Y"
    ) %>%
    dplyr::arrange(playerID, yearID) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(
      appearance_number = dplyr::row_number(),
      is_first_ballot   = as.integer(appearance_number == 1L),
      prev_share        = dplyr::lag(share, 1),
      prev2_share       = dplyr::lag(share, 2),
      prev_delta        = prev_share - prev2_share,
      max_share_prior   = dplyr::lag(cummax(share)),   # best share strictly before this year
      years_remaining   = bbwaa_ballot_max_years(yearID) - appearance_number
    ) %>%
    dplyr::ungroup() %>%
    # ---- ballot strength (Rule-of-10 crowding), from PRE-VOTE info only ----
    # competitor_mass = returning vote demand of the REST of the field (others'
    # prior-year shares); a weak/uncrowded ballot lets borderline candidates rise.
    dplyr::group_by(yearID) %>%
    dplyr::mutate(
      n_cand               = dplyr::n(),
      competitor_mass      = sum(prev_share, na.rm = TRUE) - dplyr::coalesce(prev_share, 0),
      n_strong_competitors = sum(prev_share >= 0.5, na.rm = TRUE) -
                               as.integer(dplyr::coalesce(prev_share, 0) >= 0.5)
    ) %>%
    dplyr::ungroup()
}

#' Per-player ballot state ENTERING `target_year`, from appearances before it.
#'
#' Used to build the score rows for the cycle being predicted: a holdover's
#' `prev_share` is their most recent completed-election share, `appearance_number`
#' is (prior appearances + 1), etc. Players with no prior appearance (first-timers)
#' are absent here and are defaulted by build_bbwaa_score().
ballot_state_asof <- function(panel, target_year) {
  panel %>%
    dplyr::filter(yearID < target_year) %>%
    dplyr::group_by(playerID) %>%
    dplyr::arrange(yearID, .by_group = TRUE) %>%
    dplyr::summarise(
      n_prior         = dplyr::n(),
      prev_share      = dplyr::last(share),
      prev2_share     = dplyr::nth(share, -2L),   # NA when only one prior appearance
      max_share_prior = max(share),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      appearance_number = n_prior + 1L,
      is_first_ballot   = 0L,
      prev_delta        = prev_share - prev2_share,
      years_remaining   = bbwaa_ballot_max_years(target_year) - appearance_number
    )
}

#' Build the career-merit feature matrix for one player type across ALL eras.
#'
#' Reuses the exact any-route feature builders (assemble_* -> get_fit_data ->
#' career aggregates), but over a full-history final-game window so every balloted
#' player is covered (not the any-route temporal train/test windows). The any-route
#' `inducted` label is dropped — it encodes the outcome and would leak.
build_merit_matrix <- function(war, player_type = c("batter", "pitcher"), cfg) {
  player_type <- match.arg(player_type)
  lmy      <- cfg$dates$label_max_yr
  wide_max <- sprintf("%d-12-31", cfg$data_season)   # cover everyone through the data season

  if (player_type == "batter") {
    m <- get_fit_data(assemble_batters(war, label_max_year = lmy),
                      final_game_min = "1901-01-01", final_game_max = wide_max) %>%
      dplyr::filter(POS %notin% c("SP", "RP"))
    m <- m %>%
      dplyr::inner_join(get_aggregated_fit_df(m), by = "playerID") %>%
      dplyr::select(-dplyr::starts_with(.BBWAA_BAT_DROP))
  } else {
    m <- p_get_fit_data(assemble_pitchers(war, label_max_year = lmy),
                        final_game_min = "1901-01-01", final_game_max = wide_max) %>%
      dplyr::filter(POS %in% c("SP", "RP"))
    m <- m %>%
      dplyr::inner_join(p_get_aggregated_fit_df(m), by = "playerID") %>%
      dplyr::select(-dplyr::starts_with(.BBWAA_PIT_DROP))
  }
  m %>% dplyr::select(-dplyr::any_of("inducted"))
}
