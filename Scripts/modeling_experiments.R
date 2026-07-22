#!/usr/bin/env Rscript
# Phase 6 validation: compare the modeling options on batters (CV out-of-fold
# metrics), and sanity-check the experimental 4-class model. Binary/none is the
# shipped default; this shows the alternatives work and how they compare.
#   Rscript scripts/modeling_experiments.R
suppressMessages({
  for (f in c("data_snapshot","config","data_supplement","tables","data_live",
              "feature_matrix","features","assemble","cohorts","model","evaluate",
              "labels","pipeline"))
    source(file.path("R", paste0(f, ".R")))
  library(dplyr); library(readr)
})

cfg <- load_config("config.yml"); init_tables(cfg)
war <- load_war(cfg); cohorts <- build_cohorts(cfg)
split <- build_split_batters(suppressWarnings(suppressMessages(assemble_batters(
           war, label_max_year = cfg$dates$label_max_yr))), cohorts, cfg)
seed <- cfg$seeds$batter

row <- function(tag, m) sprintf("%-16s AUC %.3f | PR-AUC %.3f | logloss %.3f | Brier %.3f | P@%d %.3f",
                                tag, m$roc_auc, m$pr_auc, m$log_loss, m$brier, m$n_pos, m$precision_at_k)

cat("\n== Batter binary model — CV out-of-fold metrics by option ==\n")
base <- train_hof_model(split$train, seed, cfg$model$cv_folds, "xgbLinear", "none")
mb <- evaluate_model(base); cat(row("baseline", mb), "\n")

up  <- train_hof_model(split$train, seed, cfg$model$cv_folds, "xgbLinear", "up")
mu  <- evaluate_model(up);  cat(row("imbalance=up", mu), "\n")

cat("\n== Calibration (Platt) on the baseline model ==\n")
cal <- fit_calibrator(base, "platt")
oof <- base$pred; y <- as.integer(oof$obs == "Y")
cat(sprintf("Brier  raw %.4f -> platt %.4f\n", brier(y, oof$Y), brier(y, apply_calibrator(cal, oof$Y))))
cat(sprintf("logloss raw %.4f -> platt %.4f\n", log_loss(y, oof$Y), log_loss(y, apply_calibrator(cal, oof$Y))))

cat("\n== Experimental 4-class model ==\n")
train4 <- add_hof_class(split$train, cfg$dates$label_max_yr)
cat("train class counts:", paste(names(table(train4$hof_class)), table(train4$hof_class), collapse="  "), "\n")
mc <- train_hof_multiclass(train4, seed, cfg$model$cv_folds, "xgbLinear")
em <- evaluate_multiclass(mc)
cat(sprintf("macro one-vs-rest AUC: %.3f\n", em$macro_auc))
cat("per-class AUC:", paste(names(em$per_class_auc), unlist(em$per_class_auc), collapse="  "), "\n")
pr <- predict_hof_multiclass(mc, split$test)
ppl <- get_table(cfg,"People") %>% transmute(playerID, name=paste(nameFirst,nameLast))
cat("\nTop 10 by P(FirstBallot) in test:\n")
print(pr %>% left_join(ppl, by="playerID") %>%
        transmute(name, P_FirstBallot=round(P_FirstBallot,3), P_BBWAA=round(P_BBWAA,3),
                  P_VetCom=round(P_VetCom,3), predict_class) %>%
        arrange(desc(P_FirstBallot)) %>% head(10) %>% as.data.frame())
cat("\nDONE\n")
