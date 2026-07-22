# =============================================================================
# Cohorts — special player sets consumed by the temporal split & reporting:
#   * VetCom-eligible: fell off the BBWAA ballot (>=10 yrs), never inducted.
#   * BBWAA ballot: the specific per-cycle ballot (a manual input — cannot be
#     derived from Lahman; Phase 3 moves it to Data/ballots/ballot_<season>.yml).
# Ported from "Pull External Baseball Data-2024.R" + pull_hof_vet_eligible().
# Dates are still hardcoded here (parity); Phase 3 wires them to cfg$dates.
# =============================================================================

if (!exists("get_lahman_batting")) source(file.path("R", "features.R"))

# The 2026 ballot (short 7-char IDs), resolved to full playerIDs by find_hof_ballot_ids().
.ballot_ids_26 <- c(
  # First-time candidates
  "braunry","hamelco","porceri","choosh0","encared","gordoal","gonzagi","kempma0",
  "markani","kendrho","murphda","pencehu",
  # Returning candidates
  "beltrca","jonean0","rodrial","ramirma","utleych","pettian","hernafe","abreubo",
  "pedrodu","rolliji","vizquom","buehrma","rodrifr","wrighda","hunteto"
)

#' Resolve short ballot IDs to full playerIDs (latest player <= last_year per stub).
find_hof_ballot_ids <- function(l = .ballot_ids_26, last_year = 2020, rem_ids = c(), add_ids = c()) {
  resolve <- function(df) {
    df %>%
      mutate(short_playerID = substr(playerID, 1, 7)) %>%
      filter(short_playerID %in% l) %>%
      group_by(playerID, short_playerID) %>%
      summarise(maxYear = max(yearID), .groups = "drop") %>%
      filter(maxYear <= last_year) %>%
      group_by(short_playerID) %>%
      filter(maxYear == max(maxYear)) %>%
      ungroup() %>%
      arrange(playerID)
  }
  bat_df <- resolve(get_lahman_batting())
  pit_df <- resolve(get_lahman_pitching())
  all_ids <- unique(c(bat_df$playerID, pit_df$playerID))
  all_ids <- setdiff(all_ids, rem_ids)
  c(all_ids, add_ids)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Read the per-cycle ballot file (Data/ballots/<ballot_file>), or NULL if none
#' is configured. Externalises the previously-hardcoded ballot stubs (config.yml
#' `ballot_file`); resolution stays identical, so the any-route split is unchanged.
read_ballot_file <- function(cfg) {
  if (is.null(cfg$ballot_file)) return(NULL)
  path <- file.path(cfg$paths$data, "ballots", cfg$ballot_file)
  if (!file.exists(path)) stop(sprintf("ballot_file not found: %s", path), call. = FALSE)
  y <- yaml::read_yaml(path)
  # A DERIVED ballot (Scripts/project_ballots.R) carries full playerIDs directly
  # under `full_ids` (either a flat list or a nested {holdovers, first_timers}
  # block) — no 7-char stub resolution needed. Manual/historical ballots keep the
  # `short_ids` path. `full_ids` wins when present.
  full <- if (!is.null(y$full_ids)) unlist(y$full_ids, use.names = FALSE) else NULL
  list(
    full_ids  = full,
    short_ids = unlist(y$short_ids),
    last_year = y$last_year %||% cfg$dates$ballot_last_yr,
    rem_ids   = unlist(y$remove_ids) %||% character(0),
    add_ids   = unlist(y$add_ids)    %||% character(0)
  )
}

#' Build the cohort sets used by the split & reporting (dates from cfg$dates).
build_cohorts <- function(cfg) {
  bf <- read_ballot_file(cfg)
  ballot <- if (is.null(bf)) {
    # fallback: the hardcoded 2026 ballot (kept for back-compat when no file set)
    find_hof_ballot_ids(
      last_year = cfg$dates$ballot_last_yr,
      rem_ids   = c("hunteto03", "braunry01", "wrighda04", "murphda07"),
      add_ids   = c("jonesan01")
    )
  } else if (!is.null(bf$full_ids)) {
    # derived ballot: full playerIDs already resolved; just apply overrides
    unique(c(setdiff(bf$full_ids, bf$rem_ids), bf$add_ids))
  } else {
    find_hof_ballot_ids(l = bf$short_ids, last_year = bf$last_year,
                        rem_ids = bf$rem_ids, add_ids = bf$add_ids)
  }
  list(
    vetcom = pull_hof_vet_eligible(
      vetcom_years     = cfg$dates$vetcom_years,
      ballot_max_years = cfg$thresholds$ballot_max_years
    ),
    ballot = ballot
  )
}
