# Evaluation-metric unit tests (pure functions) + 4-class label derivation.
setwd(PROJ_ROOT)

test_that("AUC-ROC is correct on simple cases", {
  expect_equal(auc_roc(c(1, 1, 0, 0), c(.9, .8, .4, .1)), 1)          # perfect
  expect_equal(auc_roc(c(1, 1, 0, 0), c(.9, .2, .8, .1)), 0.75)       # 3 of 4 pairs
  expect_true(is.na(auc_roc(c(1, 1, 1), c(.5, .6, .7))))              # one class
})

test_that("PR-AUC, log-loss, Brier, precision@k behave", {
  expect_equal(auc_pr(c(1, 1, 0, 0), c(.9, .8, .4, .1)), 1)          # perfect ranking
  expect_lt(log_loss(c(1, 0), c(.9, .1)), log_loss(c(1, 0), c(.6, .4)))
  expect_equal(brier(c(1, 0), c(1, 0)), 0)
  expect_equal(precision_at_k(c(1, 1, 0, 0), c(.9, .8, .4, .1), k = 2), 1)
})

test_that("derive_hof_4class labels a known first-ballot player correctly", {
  cfg <- load_config("config.yml"); init_tables(cfg)
  cls <- derive_hof_4class(label_max_year = cfg$dates$label_max_yr)
  expect_true(all(cls$hof_class %in% c("FirstBallot", "BBWAA", "VetCom")))
  # Derek Jeter was a first-ballot inductee (2020)
  expect_equal(cls$hof_class[cls$playerID == "jeterde01"], "FirstBallot")
})

test_that("engine registry and calibrators are wired", {
  expect_true(all(c("xgbLinear", "glmnet", "rf") %in% names(.ENGINES)))
  expect_null(fit_calibrator(NULL, "none"))
})
