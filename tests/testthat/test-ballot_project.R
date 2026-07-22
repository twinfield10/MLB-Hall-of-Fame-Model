# Future-cycle ballot projection tests. Guards the known-actual panel union
# (grounds a cycle beyond the snapshot without leaking into earlier runs), the
# holdover "prior-cycle only" rule (else long-retired players resurrect), the
# 10-year tenure cap, and the projected-cycle degradation no-op.
setwd(PROJ_ROOT)

test_that("read_known_results loads results_<year>.csv with a HallOfFame-shaped schema", {
  known <- read_known_results("Data")
  expect_false(is.null(known))
  expect_true(all(c("playerID", "yearID", "votedBy", "ballots", "votes", "inducted")
                  %in% names(known)))
  expect_true(2026 %in% known$yearID)
})

test_that("build_ballot_panel folds in known-actual results past the snapshot, gated by label", {
  cfg <- load_config("config.yml"); init_tables(cfg)      # season 2026, label <= 2025

  p25 <- build_ballot_panel(label_max_year = 2025)
  expect_false(any(p25$yearID == 2026))                   # real 2026 filtered out -> 2026-run parity

  p26 <- build_ballot_panel(label_max_year = 2026)
  r26 <- p26[p26$yearID == 2026, ]
  expect_gt(nrow(r26), 20)                                # the real 2026 ballot is present
  expect_true(all(c("beltrca01", "jonesan01") %in% r26$playerID[r26$elected]))
})

test_that("derive_holdovers keys off the immediately prior cycle only", {
  cfg <- list(bbwaa = list(survive_pct = 0.05, elect_pct = 0.75),
              cohorts = list(banned_ids = list()))
  panel <- data.frame(
    playerID = c("old", "old", "recent", "recent", "elected", "lowvote"),
    yearID   = c(1975,  1976,  2025,     2026,     2026,      2026),
    share    = c(0.40,  0.30,  0.20,     0.30,     0.80,      0.02),
    elected  = c(FALSE, FALSE, FALSE,    FALSE,    TRUE,      FALSE)
  )
  h <- derive_holdovers(2027, panel, cfg)
  expect_true ("recent"  %in% h)   # on the 2026 ballot, 5-75%, tenure left
  expect_false("old"     %in% h)   # last appeared 1976 -> NOT a 2027 holdover
  expect_false("elected" %in% h)   # elected in 2026
  expect_false("lowvote" %in% h)   # below the 5% survival line
})

test_that("derive_holdovers respects the 10-year ballot tenure cap", {
  cfg <- list(bbwaa = list(survive_pct = 0.05, elect_pct = 0.75),
              cohorts = list(banned_ids = list()))
  ten  <- data.frame(playerID = "ten",  yearID = 2017:2026,
                     share = rep(0.30, 10), elected = rep(FALSE, 10))
  expect_false("ten"  %in% derive_holdovers(2027, ten,  cfg))   # 10 appearances -> exhausted
  nine <- data.frame(playerID = "nine", yearID = 2018:2026,
                     share = rep(0.30, 9),  elected = rep(FALSE, 9))
  expect_true ("nine" %in% derive_holdovers(2027, nine, cfg))   # 9 -> 2027 is the 10th, allowed
})

test_that("collect_projected_cycles is a no-op once real data reaches season-1", {
  cfg <- load_config("config.yml"); init_tables(cfg)
  panel <- build_ballot_panel(label_max_year = 2026)     # real/known through 2026
  expect_null(collect_projected_cycles(cfg, panel, 2027)) # nothing to synthesise for 2027
})
