# Ballot-history feature tests (BBWAA committee model). Guards the panel
# construction, the no-future-leakage property, era ballot-tenure rules, the
# as-of state builder, and banned-id exclusion.
setwd(PROJ_ROOT)

test_that("bbwaa_ballot_max_years follows the era rules", {
  expect_equal(bbwaa_ballot_max_years(c(2020, 2000, 1950)), c(10L, 15L, 20L))
})

test_that("build_ballot_panel derives ballot history without leaking the future", {
  cfg <- load_config("config.yml"); init_tables(cfg)
  panel <- build_ballot_panel(label_max_year = cfg$dates$label_max_yr)

  expect_true(nrow(panel) > 0)
  expect_true(all(panel$yearID <= cfg$dates$label_max_yr))   # label cap respected

  # Tim Raines: 10 BBWAA appearances (2008-2017), elected in 2017.
  r <- panel[panel$playerID == "raineti01", ]
  r <- r[order(r$yearID), ]
  expect_gt(nrow(r), 5)
  expect_equal(r$appearance_number, seq_len(nrow(r)))         # 1,2,3,... in year order
  expect_equal(sum(r$is_first_ballot), 1L)                   # exactly one first ballot
  expect_true(is.na(r$prev_share[1]))                        # no prior share on debut
  # prev_share is strictly the PREVIOUS appearance's share (a lag, never the future)
  expect_equal(r$prev_share[-1], head(r$share, -1))
  # max_share_prior on the final row excludes that row's (election) share
  expect_equal(r$max_share_prior[nrow(r)], max(head(r$share, -1)))
  # elected only in the final (induction) year
  expect_equal(which(r$elected), nrow(r))
})

test_that("ballot_state_asof summarises the state entering a cycle", {
  panel <- data.frame(
    playerID = "x", yearID = c(2018, 2019, 2020),
    share = c(0.30, 0.50, 0.60), elected = c(FALSE, FALSE, FALSE)
  )
  st <- ballot_state_asof(panel, target_year = 2021)
  expect_equal(st$appearance_number, 4L)     # 3 prior appearances -> entering the 4th
  expect_equal(st$prev_share, 0.60)          # most recent completed cycle
  expect_equal(st$prev2_share, 0.50)
  expect_equal(st$max_share_prior, 0.60)
  expect_equal(st$years_remaining, bbwaa_ballot_max_years(2021) - 4L)
})

test_that("build_bbwaa_train drops permanently ineligible players", {
  cfg <- load_config("config.yml")
  panel <- data.frame(
    playerID = c("rosepe01", "aaaaaa01", "bbbbbb01"),
    yearID = 2000L, share = 0.5, elected = FALSE,
    appearance_number = 1L, is_first_ballot = 1L,
    prev_share = NA_real_, prev2_share = NA_real_,
    prev_delta = NA_real_, max_share_prior = NA_real_, years_remaining = 14L
  )
  merit <- data.frame(playerID = c("rosepe01", "aaaaaa01", "bbbbbb01"),
                      POS = factor("OF"), WAR_1 = c(1, 2, 3))
  tr <- build_bbwaa_train(panel, merit, cfg)
  expect_false("rosepe01" %in% tr$playerID)              # banned (config cohorts$banned_ids)
  expect_setequal(tr$playerID, c("aaaaaa01", "bbbbbb01"))
  expect_true(all(!is.na(tr$prev_share)))                # lagged NAs zero-filled
})
