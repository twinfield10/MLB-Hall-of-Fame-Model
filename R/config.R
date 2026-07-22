# =============================================================================
# Configuration & temporal derivation
#
# load_config() reads config.yml, reads DATA_SEASON from the snapshot MANIFEST,
# then DERIVES every temporal cutoff from `season` + `data_season`. This is the
# fix for the codebase's central problem: ~15 hardcoded years scattered across
# the old scripts. Rolling to a new season = change `season` (and drop in a new
# snapshot) — no code edits.
#
# See docstring of derive_dates() for the mapping from the old literals.
# =============================================================================

suppressMessages({
  library(yaml)
})

if (!exists("read_manifest")) source(file.path("R", "data_snapshot.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Derive every temporal value from `season` and `data_season`.
#'
#' Mapping from the old hardcoded literals (values shown @ season=2026, data_season=2024):
#'   Age = 2025 - birthYear         -> age_as_of      = data_season + 1                = 2025
#'   train final_game_max 2011-12-31-> settled_max    = season - settled_lag(15)       = 2011
#'   recent window start 2012-01-01 -> recent_min     = settled_max + 1                = 2012
#'   recent window end   2023-12-31 -> recent_max     = data_season - 1                = 2023
#'   VetCom test branch  2012-12-31 -> (off-by-one)   normalised to settled_max        = 2011
#'   Eligible <= 2018-01-01         -> eligible_max_yr= season - eligible_lag(6)        = 2020  (old 2018 was stale)
#'   Active  >= 2022 / 2024         -> active_min_yr  = data_season                     = 2024  (unifies the two)
#'   ballot last_year = 2020        -> ballot_last_yr = season - eligible_lag(6)        = 2020
#'   VetCom scan seq(1997, 2022)    -> seq(vetcom_start, hof_max_year)                  = 1997..2025
derive_dates <- function(season, data_season, thresholds, windows, hof_max_year = NULL) {
  settled_max     <- season - windows$settled_lag
  recent_min      <- settled_max + 1
  recent_max      <- data_season - 1
  eligible_max_yr <- season - windows$eligible_lag
  active_min_yr   <- data_season
  age_as_of       <- data_season + 1
  # HOF results are only "known" strictly before the season we predict, so we
  # never train/test on the outcome we're forecasting even when the snapshot's
  # HallOfFame table is a year ahead of its stats (as the 2025 release is).
  label_max_yr    <- season - 1
  vetcom_max      <- min(label_max_yr, hof_max_year %||% label_max_yr)

  list(
    # scalar years
    settled_max      = settled_max,
    recent_min       = recent_min,
    recent_max       = recent_max,
    eligible_max_yr  = eligible_max_yr,
    active_min_yr    = active_min_yr,
    age_as_of        = age_as_of,
    label_max_yr     = label_max_yr,
    ballot_last_yr   = eligible_max_yr,
    vetcom_years     = seq(thresholds$vetcom_start, vetcom_max),

    # date strings compared against People$finalGame (a Date)
    train_min_date   = "1901-01-01",
    train_max_date   = sprintf("%d-12-31", settled_max),
    recent_min_date  = sprintf("%d-01-01", recent_min),
    recent_max_date  = sprintf("%d-12-31", recent_max),
    eligible_max_date= sprintf("%d-01-01", eligible_max_yr),
    active_min_date  = sprintf("%d-01-01", active_min_yr)
  )
}

#' Validate the config + snapshot relationship. Fails loudly on incoherent input.
validate_config <- function(cfg) {
  stopifnot(
    "season must be an integer year"        = is.numeric(cfg$season) && cfg$season == round(cfg$season),
    "data_season must be present"           = is.numeric(cfg$data_season),
    "season must be >= data_season"         = cfg$season >= cfg$data_season,
    "snapshot dir must exist"               = dir.exists(snapshot_dir(cfg))
  )
  if (!is.null(cfg$supplement) &&
      !file.exists(file.path(cfg$paths$data, "supplement", cfg$supplement))) {
    stop(sprintf("supplement file not found: %s", cfg$supplement), call. = FALSE)
  }
  if (!is.null(cfg$ballot_file) &&
      !file.exists(file.path(cfg$paths$data, "ballots", cfg$ballot_file))) {
    stop(sprintf("ballot_file not found: %s", cfg$ballot_file), call. = FALSE)
  }
  invisible(TRUE)
}

#' Load config.yml, read DATA_SEASON from the snapshot manifest, derive all dates.
#' @param path      path to config.yml
#' @param season    optional override for the season knob
#' @param overrides optional named list merged over the YAML (shallow) BEFORE
#'   validation/derivation — e.g. list(max_data_lag = 5) when projecting several
#'   cycles past the stats horizon (the projection driver deliberately extrapolates
#'   the ballot beyond DATA_SEASON; first-timers still resolve within the snapshot).
load_config <- function(path = "config.yml", season = NULL, overrides = NULL) {
  cfg <- yaml::read_yaml(path)
  if (!is.null(overrides)) cfg <- utils::modifyList(cfg, overrides)
  if (!is.null(season)) cfg$season <- as.integer(season)
  cfg$season <- as.integer(cfg$season)

  # normalise nullable string fields ("null"/"" -> NULL)
  for (f in c("supplement", "ballot_file")) {
    if (is.null(cfg[[f]]) || identical(cfg[[f]], "null") || identical(cfg[[f]], "")) cfg[[f]] <- NULL
  }

  manifest <- read_manifest(cfg)
  cfg$data_season <- as.integer(manifest$data_season)
  cfg$snapshot_manifest <- manifest

  hof_max <- manifest$tables[["HallOfFame"]]$max_year
  cfg$dates <- derive_dates(cfg$season, cfg$data_season,
                            cfg$thresholds, cfg$windows,
                            hof_max_year = hof_max)

  validate_config(cfg)
  validate_vintage(cfg, manifest = manifest)   # loud fail on stale / incoherent snapshot
  cfg
}
