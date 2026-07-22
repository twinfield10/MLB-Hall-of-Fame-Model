#!/usr/bin/env Rscript
# Render the HOF likelihood report tables (HTML) from the latest predictions.
#   Rscript run.R              # produce predictions first
#   Rscript scripts/render_report.R
suppressMessages({
  for (f in c("data_snapshot","config","data_supplement","tables","report"))
    source(file.path("R", paste0(f, ".R")))
})
cfg <- load_config("config.yml"); init_tables(cfg)
dir <- render_report(cfg, top_n = 25)
message(sprintf("Report tables written to %s", dir))
