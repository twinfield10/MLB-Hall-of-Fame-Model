# Tests for the temporal derivation layer — the fix for the codebase's central
# problem (hardcoded years). Written against derive_dates() as a PURE function so
# they don't depend on which snapshot config.yml currently points at.
#
# The R/ functions assume wd = project root (as under run.R / targets). testthat
# resets wd to the test dir per file, so re-anchor it here (PROJ_ROOT from setup.R).
setwd(PROJ_ROOT)

# thresholds/windows are stable config; borrow them from whatever config.yml holds.
tw <- function() { cfg <- load_config("config.yml"); list(t = cfg$thresholds, w = cfg$windows) }

test_that("derive_dates reproduces the old literals @ season=2026, data_season=2024", {
  x <- tw(); d <- derive_dates(2026, 2024, x$t, x$w, hof_max_year = 2025)
  expect_equal(d$age_as_of, 2025)                # old: Age = 2025 - birthYear
  expect_equal(d$settled_max, 2011)              # old: train final_game_max 2011-12-31
  expect_equal(d$train_max_date, "2011-12-31")
  expect_equal(d$recent_min_date, "2012-01-01")
  expect_equal(d$recent_max_date, "2023-12-31")
  expect_equal(d$ballot_last_yr, 2020)           # old: find_hof_ballot_ids(last_year=2020)
})

test_that("intended, documented fixes are applied @ 2026/2024", {
  x <- tw(); d <- derive_dates(2026, 2024, x$t, x$w, hof_max_year = 2025)
  expect_equal(d$eligible_max_date, "2020-01-01") # was stale 2018-01-01
  expect_equal(d$active_min_date, "2024-01-01")   # unifies inconsistent 2022/2024
  expect_equal(min(d$vetcom_years), 1997)
  expect_equal(max(d$vetcom_years), 2025)
})

test_that("label cutoff never uses the prediction season's HOF results", {
  x <- tw()
  # 2025 snapshot's HallOfFame runs through 2026; predicting 2026 must cap at 2025.
  d <- derive_dates(2026, 2025, x$t, x$w, hof_max_year = 2026)
  expect_equal(d$label_max_yr, 2025)
  expect_true(max(d$vetcom_years) <= 2025)
})

test_that("season rollover shifts every window by one year (no code edits)", {
  x <- tw()
  d26 <- derive_dates(2026, 2024, x$t, x$w, hof_max_year = 2025)
  d27 <- derive_dates(2027, 2024, x$t, x$w, hof_max_year = 2025)
  expect_equal(d27$settled_max,     d26$settled_max + 1)
  expect_equal(d27$eligible_max_yr, d26$eligible_max_yr + 1)
  expect_equal(d27$ballot_last_yr,  d26$ballot_last_yr + 1)
  expect_equal(d27$label_max_yr,    d26$label_max_yr + 1)
})

test_that("load_config is internally consistent for whatever snapshot it points at", {
  cfg <- load_config("config.yml")
  expect_true(cfg$season >= cfg$data_season)
  expect_equal(cfg$dates$label_max_yr, cfg$season - 1)
  expect_equal(cfg$dates$recent_max,   cfg$data_season - 1)
  expect_true(dir.exists(snapshot_dir(cfg)))
})

test_that("stale-data tripwire fires when season is too far ahead of the snapshot", {
  ds <- load_config("config.yml")$data_season
  expect_error(load_config("config.yml", season = ds + 5), "STALE DATA")
})

test_that("validate_vintage rejects an incoherent Batting/Pitching mismatch", {
  cfg <- load_config("config.yml")
  m <- cfg$snapshot_manifest
  m$tables[["Pitching"]]$max_year <- m$tables[["Batting"]]$max_year - 1L
  expect_error(validate_vintage(cfg, manifest = m), "incoherent")
})
