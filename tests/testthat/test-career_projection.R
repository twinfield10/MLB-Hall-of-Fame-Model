# Career-projection tests (active-player Monte-Carlo forecasting). Guards the
# aging curves, the performance-aware retirement hazard, the Marcel baseline, and
# the deterministic projected-career build (adds seasons, recomputes jaws).
setwd(PROJ_ROOT)

# minimal but complete synthetic batter career (all columns project_career touches)
.mk_bat <- function(n = 5, war = 5, age0 = 28, pa = 650) {
  yrs <- seq(2026 - n, 2025)
  data.frame(
    playerID = "testp", yearID = yrs, Age = age0 + seq_len(n) - 1, PA = pa,
    WAR = war, WAA = war - 1, WAR_off = war * 0.8, WAR_def = war * 0.2,
    OPS_plus = 130, WAA_rank = 5, HOFM_Points = 8,
    H = 170, HR = 30, RBI = 95, R = 95, BB = 70, SO = 120, SB = 8, AB = 560,
    X2B = 32, X3B = 2, TB = 300, HBP = 6, SF = 5, SH = 0, GIDP = 12, IBB = 5, G = 150, CS = 3,
    POS = "OF", inducted = "N", jaws = war * n * 0.5, d_jaws = war * 0.2 * n * 0.5,
    finalGame = as.Date(sprintf("%d-10-01", max(yrs))),
    MVPWin = 0, ROYWin = 0, SS_Win = 0, GG_Win = 0, TC_Win = 0, WSWin = 0,
    PenWin = 0, DivWin = 0, AllStar = 0, AllStarStart = 0, MVPShare = 0,
    stringsAsFactors = FALSE)
}

test_that("aging multipliers decline with age and stay below the prime", {
  expect_gt(.age_mult(26, "batter"), .age_mult(35, "batter"))
  expect_gt(.age_mult(30, "pitcher"), .age_mult(38, "pitcher"))
  expect_lt(.age_mult(38, "batter"), 0.9)
})

test_that("retirement hazard is performance-aware and bounded", {
  expect_true(.retire_hazard(38, 0.6) > .retire_hazard(33, 4.0))   # old & fading > young & productive
  expect_true(.retire_hazard(45, 0.1) <= 0.85)                     # capped
  expect_true(.retire_hazard(28, 5.0) >= 0.02)                     # floored
})

test_that("Marcel baseline weights recent seasons most heavily", {
  d <- .mk_bat(n = 3); d$WAR <- c(2, 4, 8); d$PA <- c(600, 600, 600)   # rising: 8 most recent
  b <- .marcel_baseline(d, "batter")
  expect_gt(b$bWAR, mean(d$WAR))          # 5/4/3 weighting pulls toward the recent 8
  expect_true(b$bPT > 0)
})

test_that("project_career extends a productive career and recomputes jaws", {
  real <- .mk_bat(n = 6, war = 6, age0 = 27)
  comb <- project_career(real, "batter", rng = NULL)
  expect_gt(nrow(comb), nrow(real))                               # added seasons
  expect_true(all(comb$yearID[-seq_len(nrow(real))] > max(real$yearID)))  # future years
  expect_equal(comb$jaws[1], comb$jaws[nrow(comb)])              # jaws is a recomputed career scalar
  expect_gt(comb$jaws[1], real$jaws[1])                          # career JAWS grew
})

test_that("a fading 40-year-old adds few or no seasons", {
  old <- .mk_bat(n = 4, war = 0.8, age0 = 39)
  comb <- project_career(old, "batter", rng = NULL)
  expect_lte(nrow(comb) - nrow(old), 3L)
})
