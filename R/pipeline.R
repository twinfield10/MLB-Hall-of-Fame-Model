# =============================================================================
# Pipeline — feature-matrix build, temporal split, train, predict, write.
# Phase 2: mirrors the two -2024 runner scripts faithfully, with data now
# sourced from the snapshot. Temporal cutoffs are still the literals from the
# runners (parity); Phase 3 replaces them with cfg$dates, Phase 4 unifies the
# batter/pitcher paths.
# =============================================================================

if (!exists("assemble_batters")) source(file.path("R", "assemble.R"))
if (!exists("build_cohorts"))    source(file.path("R", "cohorts.R"))
if (!exists("train_hof_model"))  source(file.path("R", "model.R"))
if (!exists("evaluate_model"))   source(file.path("R", "evaluate.R"))
if (!exists("add_hof_class"))    source(file.path("R", "labels.R"))
if (!exists(".run_bbwaa"))       source(file.path("R", "committee_bbwaa.R"))

`%notin%` <- Negate(`%in%`)

# Aggregate award/season columns dropped after joining career aggregates (batters).
.BAT_DROP <- c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "ROYWin_", "TC_Win_",
               "AllStar_", "C_HOFM_Points")
.PIT_DROP <- c("MVPWin_", "GG_Win_", "CYWin_", "AllStar_", "ROYWin_", "C_HOFM_Points")

#' Build the batter train/test feature matrices (temporal split; dates from cfg$dates).
build_split_batters <- function(bb, cohorts, cfg) {
  VetCom_IDs <- cohorts$vetcom; HOF_Ballot_IDs <- cohorts$ballot
  d <- cfg$dates; min_pa <- cfg$thresholds$min_pa
  tmp <- bb %>% filter(POS %notin% c("SP", "RP"))

  fit_df_train <- rbind(
    get_fit_data(tmp, final_game_min = d$train_min_date, final_game_max = d$train_max_date) %>%
      filter(PA_1 >= min_pa & playerID %notin% VetCom_IDs & playerID %notin% HOF_Ballot_IDs),
    get_fit_data(tmp, final_game_min = d$recent_min_date, final_game_max = d$recent_max_date) %>%
      filter(inducted == "Y")
  )
  fit_df_test <- rbind(
    get_fit_data(tmp, final_game_min = d$recent_min_date, final_game_max = d$recent_max_date) %>%
      filter(PA_1 >= min_pa & inducted == "N"),
    get_fit_data(tmp, final_game_min = d$train_min_date, final_game_max = d$train_max_date) %>%
      filter(PA_1 >= min_pa & (playerID %in% VetCom_IDs | playerID == "ramirma02"))
  )

  fit_df_train <- fit_df_train %>%
    inner_join(get_aggregated_fit_df(fit_df_train), by = "playerID") %>%
    select(-starts_with(.BAT_DROP))
  fit_df_test <- fit_df_test %>%
    inner_join(get_aggregated_fit_df(fit_df_test), by = "playerID") %>%
    select(-starts_with(.BAT_DROP))

  list(train = fit_df_train, test = fit_df_test)
}

#' Build the pitcher train/test feature matrices (temporal split; dates from cfg$dates).
build_split_pitchers <- function(pp, cohorts, cfg) {
  VetCom_IDs <- cohorts$vetcom; HOF_Ballot_IDs <- cohorts$ballot
  d <- cfg$dates; min_ip <- cfg$thresholds$min_ipouts
  tmp <- pp %>% filter(POS %in% c("SP", "RP"))

  fit_df_train <- rbind(
    p_get_fit_data(tmp, final_game_min = d$train_min_date, final_game_max = d$train_max_date) %>%
      filter(IPouts_1 >= min_ip & playerID %notin% VetCom_IDs & playerID %notin% HOF_Ballot_IDs),
    p_get_fit_data(tmp, final_game_min = d$recent_min_date, final_game_max = d$recent_max_date) %>%
      filter(inducted == "Y")
  )
  fit_df_test <- rbind(
    p_get_fit_data(tmp, final_game_min = d$recent_min_date, final_game_max = d$recent_max_date) %>%
      filter(IPouts_1 >= min_ip & inducted == "N"),
    p_get_fit_data(tmp, final_game_min = d$train_min_date, final_game_max = d$train_max_date) %>%
      filter(IPouts_1 >= min_ip & (playerID %in% VetCom_IDs | playerID == "ramirma02"))
  )

  fit_df_train <- fit_df_train %>%
    inner_join(p_get_aggregated_fit_df(fit_df_train), by = "playerID") %>%
    select(-starts_with(.PIT_DROP))
  fit_df_test <- fit_df_test %>%
    inner_join(p_get_aggregated_fit_df(fit_df_test), by = "playerID") %>%
    select(-starts_with(.PIT_DROP))

  list(train = fit_df_train, test = fit_df_test)
}

#' Full pipeline for one player type: assemble -> split -> train -> predict -> write.
run_pipeline <- function(cfg, player_type = c("batter", "pitcher"),
                         war = NULL, cohorts = NULL, write = TRUE) {
  player_type <- match.arg(player_type)
  init_tables(cfg)
  if (is.null(war))     war     <- load_war(cfg)
  if (is.null(cohorts)) cohorts <- build_cohorts(cfg)

  # Committee-specific models (e.g. the BBWAA vote) take a separate path with
  # their own ballot-history features, target and outputs. Default is any_route.
  if (identical(cfg$model$committee %||% "any_route", "bbwaa")) {
    return(.run_bbwaa(cfg, player_type, war = war, cohorts = cohorts, write = write))
  }

  lmy <- cfg$dates$label_max_yr
  if (player_type == "batter") {
    split <- build_split_batters(assemble_batters(war, label_max_year = lmy), cohorts, cfg)
    seed  <- cfg$seeds$batter
    out   <- file.path(cfg$paths$outputs, "predictions",
                       sprintf("hof_predictions_batters_%d.csv", cfg$season))
  } else {
    split <- build_split_pitchers(assemble_pitchers(war, label_max_year = lmy), cohorts, cfg)
    seed  <- cfg$seeds$pitcher
    out   <- file.path(cfg$paths$outputs, "predictions",
                       sprintf("hof_predictions_pitchers_%d.csv", cfg$season))
  }

  if (identical(cfg$model$label, "multiclass_4")) {
    return(.run_multiclass(cfg, player_type, split, seed, write))
  }

  fit     <- train_hof_model(split$train, seed = seed, cv_folds = cfg$model$cv_folds,
                             engine = cfg$model$engine, imbalance = cfg$model$imbalance)
  metrics <- evaluate_model(fit)
  calibrator <- fit_calibrator(fit, cfg$model$calibration)
  res     <- predict_hof(fit, split$test, calibrator = calibrator)

  if (write) {
    if (!dir.exists(dirname(out))) dir.create(dirname(out), recursive = TRUE)
    utils::write.csv(res, out)
    save_eval(metrics, cfg, player_type)
    save_model(fit, cfg, player_type, metrics = metrics, calibrator = calibrator)
  }
  list(fit = fit, results = res, split = split, out = out,
       metrics = metrics, calibrator = calibrator)
}

#' Experimental 4-class path: relabel the (binary) split's train set, fit a
#' multinomial model, score class probabilities. Writes a *_4class.csv.
.run_multiclass <- function(cfg, player_type, split, seed, write) {
  train4 <- add_hof_class(split$train, label_max_year = cfg$dates$label_max_yr)
  fit     <- train_hof_multiclass(train4, seed = seed, cv_folds = cfg$model$cv_folds,
                                  engine = cfg$model$engine)
  metrics <- evaluate_multiclass(fit)
  res     <- predict_hof_multiclass(fit, split$test)
  out <- file.path(cfg$paths$outputs, "predictions",
                   sprintf("hof_predictions_%ss_%d_4class.csv",
                           player_type, cfg$season))
  if (write) {
    if (!dir.exists(dirname(out))) dir.create(dirname(out), recursive = TRUE)
    utils::write.csv(res, out)
  }
  list(fit = fit, results = res, split = split, out = out, metrics = metrics)
}
