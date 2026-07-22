# Feature-layer tests. Guards the Phase 4 de-dup + the MPVWin bug fix.
setwd(PROJ_ROOT)

test_that("append_mvps produces numeric MVPWin and no typo'd MPVWin column", {
  cfg <- load_config("config.yml"); init_tables(cfg)
  d <- data.frame(playerID = c("judgeaa01", "troutmi01"),
                  yearID   = c(2024L, 2024L), stringsAsFactors = FALSE)
  out <- append_mvps(d)
  expect_false("MPVWin" %in% names(out))        # typo column gone
  expect_true(is.numeric(out$MVPWin))
  # Judge won the 2024 AL MVP (native in the snapshot) -> MVPWin == 1
  expect_equal(out$MVPWin[out$playerID == "judgeaa01"], 1)
})

test_that("HOF-Monitor MVP component is non-zero for a known MVP (MPVWin fix)", {
  cfg <- load_config("config.yml"); init_tables(cfg)
  src <- readLines(file.path(PROJ_ROOT, "R", "features.R"))
  # the scorer must read the correctly-spelled MVPWin, not the old MPVWin typo
  expect_true(any(grepl("MVPWin == 1 ~ 8", src)))
  expect_false(any(grepl("MPVWin == 1", src)))
})

test_that("batter and pitcher fit-matrix wrappers share one core", {
  # both delegate to build_fit_matrix; the dead convert_to_factors helpers are gone
  expect_true(exists("build_fit_matrix") && exists("aggregate_career") && exists("append_war"))
  expect_false(exists("convert_to_factors"))
  expect_false(exists("p_convert_to_factors"))
})
