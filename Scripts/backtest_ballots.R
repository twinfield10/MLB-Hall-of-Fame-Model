#!/usr/bin/env Rscript
# =============================================================================
# Historical walk-forward backtest driver.
#
#   Rscript Scripts/backtest_ballots.R              # default 2016..2026
#   Rscript Scripts/backtest_ballots.R 2016 2026    # explicit range
#   Rscript Scripts/backtest_ballots.R 2016 2026 2000   # + Monte-Carlo sims
#
# Builds the full ballot panel + per-type merit matrix ONCE, then for each cycle
# trains on yearID < Y and scores the actual year-Y ballot (see R/backtest.R).
# Writes outputs/predictions/hof_backtest_<Y>.csv (prediction schema + actual_*
# columns + pred_hit) for the frontend's Historical tab.
# =============================================================================

setwd(Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model"))

.R_FILES <- c("data_snapshot", "config", "data_supplement", "tables", "data_live",
              "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
              "model", "evaluate", "committee_bbwaa", "backtest")
suppressMessages(for (f in .R_FILES) source(file.path("R", paste0(f, ".R"))))

args   <- commandArgs(trailingOnly = TRUE)
years  <- if (length(args) >= 2) seq(as.integer(args[1]), as.integer(args[2])) else 2016:2026
n_sims <- if (length(args) >= 3) as.integer(args[3]) else 500L

# Load once with the real season so validate_config passes; backtest_one() just
# swaps cfg$season per cycle (the scoring functions read only $season / $bbwaa).
cfg <- load_config("config.yml", overrides = list(max_data_lag = 1000L))
init_tables(cfg)
war <- load_war(cfg)

message(sprintf("Backtest %d..%d | data_season %d | n_sims %d",
                min(years), max(years), cfg$data_season, n_sims))

panel   <- build_ballot_panel()                        # full history (+ results_<Y>.csv)
actuals <- build_actuals()
merit_b <- build_merit_matrix(war, "batter",  cfg)
merit_p <- build_merit_matrix(war, "pitcher", cfg)

outdir <- file.path(cfg$paths$outputs, "predictions")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

for (Y in years) {
  res <- backtest_cycle(Y, cfg, panel, actuals, merit_b, merit_p, n_sims = n_sims)
  if (is.null(res) || !nrow(res)) { message(sprintf("  %d: no candidates, skipped", Y)); next }
  out <- file.path(outdir, sprintf("hof_backtest_%d.csv", Y))
  utils::write.csv(res, out, row.names = FALSE)
  cor_sa <- suppressWarnings(stats::cor(res$pred_share, res$actual_share, use = "complete.obs"))
  message(sprintf("  %d: %2d cand | share cor %.3f | inducted actual=%d model-called=%d hits=%d/%d | %s",
                  Y, nrow(res), cor_sa,
                  sum(res$actual_inducted), sum(res$pred_call),
                  sum(res$pred_hit), nrow(res), basename(out)))
}
message("Backtest done.")
