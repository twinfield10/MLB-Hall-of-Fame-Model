# =============================================================================
# Held-out evaluation.
#
# The legacy code reported only a TRAINING-set confusion matrix — no real
# quality signal, especially given the severe class imbalance (~a few % of
# players are inducted). Here we score the model on its cross-validated
# out-of-fold predictions (savePredictions="final" in train_hof_model), which
# are genuine held-out predictions on labelled/decided players.
#
# Metrics: ROC-AUC, PR-AUC (the right headline under imbalance), log-loss,
# Brier, a calibration table, and precision@k. Pure base R — no extra deps.
# =============================================================================

#' ROC-AUC via the Mann-Whitney U statistic (handles ties).
auc_roc <- function(y, p) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Area under the precision-recall curve (step/trapezoid over sorted scores).
auc_pr <- function(y, p) {
  n1 <- sum(y == 1); if (n1 == 0) return(NA_real_)
  o <- order(p, decreasing = TRUE); yy <- y[o]
  tp <- cumsum(yy == 1); fp <- cumsum(yy == 0)
  recall    <- tp / n1
  precision <- tp / (tp + fp)
  recall <- c(0, recall); precision <- c(1, precision)
  sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2)
}

log_loss <- function(y, p, eps = 1e-15) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(y, p) mean((p - y)^2)

#' Precision among the top-k highest-scored (k defaults to the positive count).
precision_at_k <- function(y, p, k = sum(y == 1)) {
  if (k <= 0) return(NA_real_)
  mean(y[order(p, decreasing = TRUE)][seq_len(min(k, length(y)))] == 1)
}

#' Reliability table: mean predicted vs observed rate per probability bin.
calibration_table <- function(y, p, bins = 10) {
  br <- seq(0, 1, length.out = bins + 1)
  g  <- cut(p, breaks = br, include.lowest = TRUE)
  data.frame(
    bin      = levels(g),
    n        = as.integer(table(g)),
    mean_pred = as.numeric(tapply(p, g, mean)),
    obs_rate  = as.numeric(tapply(y, g, mean))
  )
}

#' Evaluate a caret fit on its out-of-fold predictions. Returns a named list.
evaluate_model <- function(fit) {
  oof <- fit$pred
  if (is.null(oof) || !nrow(oof)) stop("fit has no saved OOF predictions", call. = FALSE)
  y <- as.integer(oof$obs == "Y")
  p <- oof$Y
  cm <- caret::confusionMatrix(oof$pred, oof$obs, positive = "Y")
  list(
    n            = length(y),
    n_pos        = sum(y == 1),
    prevalence   = mean(y),
    roc_auc      = auc_roc(y, p),
    pr_auc       = auc_pr(y, p),
    log_loss     = log_loss(y, p),
    brier        = brier(y, p),
    precision_at_k = precision_at_k(y, p),
    sensitivity  = unname(cm$byClass["Sensitivity"]),
    specificity  = unname(cm$byClass["Specificity"]),
    precision    = unname(cm$byClass["Pos Pred Value"]),
    f1           = unname(cm$byClass["F1"]),
    calibration  = calibration_table(y, p)
  )
}

#' Persist evaluation metrics next to the predictions.
save_eval <- function(metrics, cfg, player_type) {
  dir <- file.path(cfg$paths$outputs, "metrics")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  flat <- metrics; flat$calibration <- NULL
  yaml::write_yaml(flat, file.path(dir, sprintf("eval_%s_%d.yml", player_type, cfg$season)))
  utils::write.csv(metrics$calibration,
                   file.path(dir, sprintf("calibration_%s_%d.csv", player_type, cfg$season)),
                   row.names = FALSE)
  invisible(dir)
}

#' One-line metric summary for logs.
format_metrics <- function(m) {
  sprintf("AUC %.3f | PR-AUC %.3f | logloss %.3f | Brier %.3f | P@%d %.3f | n=%d pos=%d",
          m$roc_auc, m$pr_auc, m$log_loss, m$brier, m$n_pos, m$precision_at_k, m$n, m$n_pos)
}
