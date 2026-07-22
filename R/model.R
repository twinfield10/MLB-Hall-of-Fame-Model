# =============================================================================
# Model training, calibration, prediction & persistence.
#
# One parameterised interface wraps caret behind an engine registry. Cross-
# validated out-of-fold predictions are saved so evaluate.R can score the model
# honestly (see R/evaluate.R). Class-imbalance handling and probability
# calibration are config-gated (default off => byte-identical to Phase 4).
# =============================================================================

suppressMessages({
  library(caret); library(xgboost); library(doParallel)
  library(dplyr); library(tidyr)
})

.hof_formula <- stats::as.formula(inducted ~ . - playerID)

# Engine registry: config `model.engine` -> caret method. xgbLinear is the
# parity default; the others are available for experimentation / comparison.
.ENGINES <- list(
  xgbLinear = "xgbLinear",
  xgbTree   = "xgbTree",
  glmnet    = "glmnet",
  rf        = "rf"
)

#' Design matrix + response for the HOF model (drops the intercept column).
hof_design <- function(df) {
  list(x = stats::model.matrix(.hof_formula, data = df)[, -1], y = df$inducted)
}

#' Train the HOF model with k-fold CV, saving out-of-fold predictions for eval.
#' @param engine    one of names(.ENGINES)
#' @param imbalance "none" | "up" | "down" | "smote" | "weights"
train_hof_model <- function(train_df, seed, cv_folds = 10,
                            engine = "xgbLinear", imbalance = "none") {
  method <- .ENGINES[[engine]]
  if (is.null(method)) stop(sprintf("unknown engine '%s'", engine), call. = FALSE)

  d  <- hof_design(train_df)
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  set.seed(seed)
  ctrl_args <- list(method = "cv", number = cv_folds,
                    classProbs = TRUE, savePredictions = "final")
  if (imbalance %in% c("up", "down", "smote")) ctrl_args$sampling <- imbalance
  ctrl <- do.call(caret::trainControl, ctrl_args)

  args <- list(x = d$x, y = d$y, method = method, trControl = ctrl, verbose = FALSE)
  if (identical(imbalance, "weights")) {
    npos <- sum(d$y == "Y"); nneg <- sum(d$y == "N")
    args$weights <- ifelse(d$y == "Y", nneg / max(npos, 1), 1)
  }
  fit <- do.call(caret::train, args)
  attr(fit, "feature_names") <- colnames(d$x)
  fit
}

# ---- probability calibration (config-gated; Phase 6) ------------------------

#' Fit a calibrator from the model's out-of-fold predictions.
#' @param method "none" | "platt" | "isotonic"
fit_calibrator <- function(fit, method = "none") {
  if (identical(method, "none")) return(NULL)
  oof <- fit$pred
  y <- as.integer(oof$obs == "Y"); p <- oof$Y
  if (identical(method, "platt")) {
    g <- stats::glm(y ~ p, family = stats::binomial())
    structure(list(method = "platt", model = g), class = "hof_calibrator")
  } else if (identical(method, "isotonic")) {
    o <- order(p); ir <- stats::isoreg(p[o], y[o])
    f <- stats::approxfun(ir$x, ir$yf, method = "linear", rule = 2, ties = "ordered")
    structure(list(method = "isotonic", fun = f), class = "hof_calibrator")
  } else stop(sprintf("unknown calibration '%s'", method), call. = FALSE)
}

#' Apply a calibrator (or pass through if NULL) to raw Y-probabilities.
apply_calibrator <- function(calibrator, p) {
  if (is.null(calibrator)) return(p)
  if (calibrator$method == "platt") {
    as.numeric(stats::predict(calibrator$model, newdata = data.frame(p = p), type = "response"))
  } else {
    pmin(pmax(calibrator$fun(p), 0), 1)
  }
}

# ---- prediction -------------------------------------------------------------

#' Score a test frame: add hard `predict` (Y/N) and (optionally calibrated)
#' `HOF_Prob`. `threshold` applies to the calibrated probability.
predict_hof <- function(fit, test_df, calibrator = NULL, threshold = NULL) {
  xx_test <- stats::model.matrix(.hof_formula, data = test_df)[, -1]
  raw  <- stats::predict(fit, newdata = xx_test, type = "prob")[, "Y"]
  prob <- apply_calibrator(calibrator, raw)
  out <- test_df
  # column order matches the legacy output: `predict` then `HOF_Prob`.
  if (is.null(threshold)) {
    hard <- stats::predict(fit, newdata = xx_test)         # caret's hard class (legacy behaviour)
    out$predict <- ifelse(hard == "Y", "Y", "N")
  } else {
    out$predict <- ifelse(prob >= threshold, "Y", "N")
  }
  out$HOF_Prob <- prob
  out
}

# ---- persistence ------------------------------------------------------------

#' Persist a fitted model plus a metadata sidecar (season, data vintage,
#' snapshot checksum, seed, engine, feature columns, class counts, metrics).
save_model <- function(fit, cfg, player_type, metrics = NULL, calibrator = NULL) {
  dir <- file.path(cfg$paths$outputs, "models")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  base <- file.path(dir, sprintf("%s_%d", player_type, cfg$season))

  saveRDS(list(fit = fit, calibrator = calibrator), paste0(base, ".rds"))

  meta <- list(
    player_type   = player_type,
    season        = cfg$season,
    data_season   = cfg$data_season,
    snapshot      = cfg$snapshot,
    label_max_yr  = cfg$dates$label_max_yr,
    engine        = cfg$model$engine,
    imbalance     = cfg$model$imbalance,
    calibration   = cfg$model$calibration,
    seed          = cfg$seeds[[player_type]],
    n_features    = length(attr(fit, "feature_names")),
    feature_names = attr(fit, "feature_names"),
    best_tune     = as.list(fit$bestTune),
    metrics       = metrics,
    trained_on    = as.character(Sys.Date()),
    r_version     = as.character(getRversion())
  )
  yaml::write_yaml(meta, paste0(base, ".meta.yml"))
  invisible(base)
}
