#!/usr/bin/env Rscript
# Quick human-readable view of the latest predictions: overall top-N, the
# BBWAA ballot cohort, and notable active players (now scored after the
# finalGame fix). Usage: Rscript scripts/show_predictions.R
suppressMessages({
  for (f in c("data_snapshot","config","data_supplement","tables"))
    source(file.path("R", paste0(f, ".R")))
  library(dplyr); library(readr)
})

cfg <- load_config("config.yml"); init_tables(cfg)
ppl <- get_table(cfg, "People") %>%
  transmute(playerID, name = paste(nameFirst, nameLast), finalGame,
            active = is.na(finalGame))

show <- function(kind, path) {
  if (!file.exists(path)) { cat("missing:", path, "\n"); return(invisible()) }
  p <- suppressWarnings(read_csv(path, show_col_types = FALSE)) %>%
    left_join(ppl, by = "playerID")
  cat(sprintf("\n================= %s (%d scored, %d predicted HOF) =================\n",
              kind, nrow(p), sum(p$predict == "Y")))
  cat("-- Top 15 by HOF_Prob --\n")
  print(p %>% arrange(desc(HOF_Prob)) %>%
          transmute(name, HOF_Prob = round(HOF_Prob, 3), predict, active) %>%
          head(15) %>% as.data.frame())
  cat("-- Top 10 ACTIVE players by HOF_Prob (newly scored) --\n")
  print(p %>% filter(active) %>% arrange(desc(HOF_Prob)) %>%
          transmute(name, HOF_Prob = round(HOF_Prob, 3), predict) %>%
          head(10) %>% as.data.frame())
}

show("BATTERS",  "outputs/predictions/hof_predictions_batters_2026.csv")
show("PITCHERS", "outputs/predictions/hof_predictions_pitchers_2026.csv")
