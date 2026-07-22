# =============================================================================
# 4-class HOF label (Phase 6, experimental — the TODO the legacy code carried).
#
# Instead of a binary inducted Y/N, distinguish HOW a player was elected:
#   FirstBallot | BBWAA | VetCom | No
# The binary temporal split is reused verbatim; we only RELABEL the (decided)
# training players, then fit a multinomial model and score class probabilities.
# Binary remains the shipped default; this path is opt-in via model.label.
# =============================================================================

suppressMessages({ library(dplyr); library(caret) })

HOF_CLASS_LEVELS <- c("No", "VetCom", "BBWAA", "FirstBallot")

#' Per-player election class for inducted players (others default to "No").
#' First-ballot = elected by the BBWAA in the player's first year on that ballot.
derive_hof_4class <- function(label_max_year = NULL) {
  hof <- tbl("HallOfFame")
  if (!is.null(label_max_year)) hof <- dplyr::filter(hof, yearID <= label_max_year)
  hof <- dplyr::filter(hof, category == "Player")

  inducted <- hof %>%
    dplyr::filter(inducted == "Y") %>%
    dplyr::group_by(playerID) %>%
    dplyr::summarise(ind_year = min(yearID),
                     ind_votedBy = votedBy[which.min(yearID)], .groups = "drop")
  first_bbwaa <- hof %>%
    dplyr::filter(votedBy %in% c("BBWAA", "Special Election")) %>%
    dplyr::group_by(playerID) %>%
    dplyr::summarise(first_bbwaa = min(yearID), .groups = "drop")

  inducted %>%
    dplyr::left_join(first_bbwaa, by = "playerID") %>%
    dplyr::mutate(hof_class = dplyr::case_when(
      ind_votedBy %in% c("BBWAA", "Special Election") &
        !is.na(first_bbwaa) & ind_year == first_bbwaa ~ "FirstBallot",
      ind_votedBy %in% c("BBWAA", "Special Election")  ~ "BBWAA",
      TRUE                                             ~ "VetCom"
    )) %>%
    dplyr::select(playerID, hof_class)
}

#' Add a 4-level `hof_class` factor to a fit matrix (inducted N -> "No").
add_hof_class <- function(fit_df, label_max_year = NULL) {
  cls <- derive_hof_4class(label_max_year)
  fit_df %>%
    dplyr::left_join(cls, by = "playerID") %>%
    dplyr::mutate(hof_class = factor(ifelse(is.na(hof_class), "No", hof_class),
                                     levels = HOF_CLASS_LEVELS))
}

.mc_formula <- stats::as.formula(hof_class ~ . - playerID - inducted)

#' Train a multinomial HOF model (drops `inducted` to avoid target leakage).
train_hof_multiclass <- function(train_df, seed, cv_folds = 10, engine = "xgbLinear") {
  method <- .ENGINES[[engine]]
  x <- stats::model.matrix(.mc_formula, data = train_df)[, -1]
  y <- train_df$hof_class
  cl <- parallel::makeCluster(parallel::detectCores()); doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  set.seed(seed)
  ctrl <- caret::trainControl(method = "cv", number = cv_folds,
                              classProbs = TRUE, savePredictions = "final")
  fit <- caret::train(x, y, method = method, trControl = ctrl, verbose = FALSE)
  attr(fit, "feature_names") <- colnames(x)
  fit
}

#' Score class probabilities on a test frame; hard `predict` = argmax class.
predict_hof_multiclass <- function(fit, test_df) {
  # add a dummy response so `.` in .mc_formula expands consistently with training
  if (!"hof_class" %in% names(test_df)) {
    test_df$hof_class <- factor(HOF_CLASS_LEVELS[1], levels = HOF_CLASS_LEVELS)
  }
  x <- stats::model.matrix(.mc_formula, data = test_df)[, -1]
  probs <- stats::predict(fit, newdata = x, type = "prob")
  names(probs) <- paste0("P_", names(probs))
  out <- test_df
  out$predict_class <- as.character(stats::predict(fit, newdata = x))
  out$HOF_Prob <- 1 - probs$P_No                # P(any induction)
  cbind(out, probs)
}

#' Macro one-vs-rest ROC-AUC over the out-of-fold predictions.
evaluate_multiclass <- function(fit) {
  oof <- fit$pred
  aucs <- sapply(HOF_CLASS_LEVELS, function(lv) {
    if (!lv %in% names(oof)) return(NA_real_)
    auc_roc(as.integer(oof$obs == lv), oof[[lv]])
  })
  list(n = nrow(oof), macro_auc = mean(aucs, na.rm = TRUE),
       per_class_auc = as.list(round(aucs, 4)),
       class_counts = as.list(table(oof$obs)))
}
