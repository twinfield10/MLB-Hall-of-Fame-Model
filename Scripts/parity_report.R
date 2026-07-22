#!/usr/bin/env Rscript
# =============================================================================
# Parity report — compares the refactored pipeline's predictions
# (outputs/predictions/) against the frozen golden fixtures (tests/golden/).
#
# This checks the ANY-ROUTE model (the refactor's parity target). Since the default
# committee is now `bbwaa`, first regenerate its predictions with the any_route model
# (e.g. temporarily set model.committee: any_route in config.yml, or Rscript -e with an
# override) so outputs/predictions/hof_predictions_*.csv exist, then run this script.
#
# Parity is measured on players present in BOTH test sets: HOF_Prob correlation
# and predict (Y/N) agreement. Membership differs by design (the snapshot marks
# active players finalGame=NA, so they leave the "retired" test window; golden's
# Lahman-package vintage gave them a 2023 finalGame). Those deltas are expected.
# =============================================================================

suppressMessages({ library(dplyr); library(readr) })

compare <- function(kind, new_path, gold_path) {
  new  <- suppressWarnings(read_csv(new_path,  show_col_types = FALSE))
  gold <- suppressWarnings(read_csv(gold_path, show_col_types = FALSE))
  common <- intersect(gold$playerID, new$playerID)
  m <- inner_join(
    gold %>% transmute(playerID, gprob = HOF_Prob, gpred = predict),
    new  %>% transmute(playerID, nprob = HOF_Prob, npred = predict),
    by = "playerID"
  )
  cat(sprintf("\n=== %s ===\n", kind))
  cat(sprintf("rows: golden %d / new %d | common %d | golden-only %d | new-only %d\n",
              nrow(gold), nrow(new), length(common),
              length(setdiff(gold$playerID, new$playerID)),
              length(setdiff(new$playerID, gold$playerID))))
  cat(sprintf("predict=Y: golden %d / new %d\n", sum(gold$predict=="Y"), sum(new$predict=="Y")))
  cat(sprintf("HOF_Prob correlation (common): %.4f\n", cor(m$gprob, m$nprob)))
  cat(sprintf("predict agreement (common):    %.4f  (%d flips)\n",
              mean(m$gpred == m$npred), sum(m$gpred != m$npred)))
  invisible(m)
}

compare("BATTERS",  "outputs/predictions/hof_predictions_batters_2026.csv",
                    "tests/golden/golden_batters_2026.csv")
compare("PITCHERS", "outputs/predictions/hof_predictions_pitchers_2026.csv",
                    "tests/golden/golden_pitchers_2026.csv")
