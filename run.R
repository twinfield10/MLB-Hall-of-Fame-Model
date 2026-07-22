#!/usr/bin/env Rscript
# =============================================================================
# One-command entry point.
#   Rscript run.R              # uses season from config.yml
#   Rscript run.R 2027         # override season
#
# Builds features from the configured snapshot, trains the batter & pitcher
# models, and writes predictions to outputs/predictions/ and models to
# outputs/models/. A {targets} DAG (Phase 7) will supersede this with caching.
# =============================================================================

# Source the function library in dependency order.
.R_FILES <- c("data_snapshot", "config", "data_supplement", "tables", "data_live",
              "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
              "model", "evaluate", "committee_bbwaa", "pipeline")
suppressMessages(for (f in .R_FILES) source(file.path("R", paste0(f, ".R"))))

args   <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[1]) else NULL

cfg <- load_config("config.yml", season = season)
message(sprintf("Season %d | data_season %d | snapshot %s | label<=%d",
                cfg$season, cfg$data_season, cfg$snapshot, cfg$dates$label_max_yr))

init_tables(cfg)                  # bind the table provider before cohorts/features
war     <- load_war(cfg)          # cached unless live_refresh: true
cohorts <- build_cohorts(cfg)

is_bbwaa <- identical(cfg$model$committee %||% "any_route", "bbwaa")

for (pt in c("batter", "pitcher")) {
  message(sprintf("== %s ==", pt))
  res <- run_pipeline(cfg, pt, war = war, cohorts = cohorts)
  if (is_bbwaa) {
    message(sprintf("   wrote %s (%d candidates; share MAE %.3f | elect AUC %.3f | n=%d)",
                    res$out, nrow(res$results), res$metrics$share_mae,
                    res$metrics$elect_roc_auc, res$metrics$n))
  } else {
    message(sprintf("   wrote %s (%d players, %d predicted HOF)",
                    res$out, nrow(res$results), sum(res$results$predict == "Y")))
    message(sprintf("   eval: %s", format_metrics(res$metrics)))
  }
}
message("Done.")
