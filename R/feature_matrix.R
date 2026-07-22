# =============================================================================
# Unified feature-matrix core (Phase 4 de-duplication).
#
# The batter and pitcher paths were ~90% copy-paste (get_fit_data/p_get_fit_data,
# get_aggregated_fit_df/p_, append_br_war/p_). The genuinely-different bits are
# parameters here; get_fit_data() etc. in features.R are now thin wrappers that
# pass the batter/pitcher specifics. Behaviour is byte-identical to Phase 3
# (verified against the frozen baseline).
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr); library(magrittr); library(purrr) })

#' Reshape per-player-season rows into the wide top-N-seasons feature matrix.
#'
#' @param stats                 per-season stat columns to spread into <stat>_1.._N
#' @param season_prefix         playing-time stat used to count real seasons ("PA" | "IPouts")
#' @param has_djaws             collapse per-season d_jaws into a single d_jaws (batters only)
#' @param med_prefilter_positive drop players with a zero top-season PA before the season count (batters)
build_fit_matrix <- function(.data, stats, season_prefix,
                             has_djaws = TRUE, med_prefilter_positive = TRUE,
                             MAXYEAR = 20, ORDER_KEY = "WAA",
                             final_game_min = "1901-01-01", final_game_max = "2025-01-01") {

  kk <- .data %>% dplyr::filter(finalGame >= final_game_min, finalGame <= final_game_max)
  kk$ORDER_KEY <- kk[[ORDER_KEY]]
  kk <- kk %>%
    dplyr::arrange(playerID, -ORDER_KEY) %>%
    dplyr::select(-ORDER_KEY) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(nyear = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nyear >= 1, nyear <= MAXYEAR)
  kk <- kk[, c("playerID", "yearID", "nyear", stats, "POS", "inducted")]

  col_numerics <- names(kk)[sapply(kk, is.numeric)]
  col_factors  <- names(kk)[sapply(kk, is.factor)]

  kkg <- kk %>%
    tidyr::gather(key, value, -playerID, -yearID, -nyear, -POS, -inducted) %>%
    mutate(new_key = paste(key, nyear, sep = "_"))
  kkg %<>% select(-key, -nyear, -yearID) %>% tidyr::spread(new_key, value, fill = 0)

  # gather() casts everything to character; restore numeric/factor by column prefix.
  for (s in col_numerics) kkg %<>% mutate_each(funs(as.numeric), starts_with(s))
  for (s in col_factors)  kkg %<>% mutate_each(funs(as.factor),  starts_with(s))

  # absent seasons spread to the spurious factor level "0"; remap it to "N".
  col_factors <- names(kkg)[sapply(kkg, is.factor)]
  for (s in col_factors) {
    if ("0" %in% levels(kkg[[s]])) {
      kkg[kkg[[s]] == "0", ][[s]] <- "N"
      kkg[[s]] <- droplevels(kkg)[[s]]
    }
  }

  # per-player count of real (non-zero) seasons by the primary playing-time stat.
  med_base <- kkg
  if (med_prefilter_positive) med_base <- med_base %>% filter(PA_1 > 0)
  med_df <- med_base %>%
    select(-starts_with(c("WAR_off_", "WAR_def_"))) %>%   # no-op for pitchers (absent)
    rowwise() %>%
    mutate(Season_Vec = pmap(across(starts_with(paste0(season_prefix, "_"))),
                             ~ c(...) %>% discard(~ . == 0)),
           Seasons = length(Season_Vec)) %>%
    select(playerID, Seasons)

  out <- kkg %>%
    left_join(med_df, by = "playerID") %>%
    filter(playerID != "rosepe01", playerID != "jacksjo01") %>%  # permanently ineligible
    mutate(
      TookSteroids  = pmax(!!!select(., starts_with("TookSteroids_")), na.rm = TRUE),
      jaws          = pmax(!!!select(., starts_with("jaws_")),         na.rm = TRUE),
      C_HOFM_Points = pmax(!!!select(., starts_with("C_HOFM_Points")), na.rm = TRUE)
    )
  if (has_djaws) {
    out <- out %>% mutate(d_jaws = pmax(!!!select(., starts_with("d_jaws_")), na.rm = TRUE))
  }
  out %>% select(-starts_with(c("TookSteroids_", "jaws_", "o_jaws_", "d_jaws_", "C_HOFM_Points_")))
}

#' Sum per-season award/points columns into career aggregates (batter & pitcher).
aggregate_career <- function(fit_df, keys_to_aggregate,
                             keys_to_keep = c("^playerID$", "^C_HOFM_Points$")) {
  nn <- names(fit_df)
  dfX <- NULL
  for (k in keys_to_keep) {
    piece <- fit_df[, grepl(k, nn), drop = FALSE]
    dfX <- if (is.null(dfX)) piece else cbind.data.frame(dfX, piece)
  }
  for (k in keys_to_aggregate) {
    nk <- stringr::str_replace_all(k, "\\^|_", "")
    dfX[[nk]] <- rowSums(fit_df[, grep(k, names(fit_df)), drop = FALSE])
  }
  dfX %>%
    mutate(HOFM_Points = C_HOFM_Points + HOFMPoints) %>%
    select(-HOFMPoints, -C_HOFM_Points)
}

#' Attach Baseball-Reference WAR (per-season, with within-year WAA rank).
#' extra_cols: batter c("WAR_off","WAR_def","OPS_plus"); pitcher c("ERA_plus").
append_war <- function(.data, war, extra_cols) {
  war %>%
    dplyr::select(player_ID, WAA, WAR, dplyr::all_of(extra_cols), year_ID, stint_ID) %>%
    combine_war_stints() %>%
    dplyr::rename(bbrefID = player_ID, yearID = year_ID) %>%
    dplyr::group_by(yearID) %>%
    dplyr::arrange(-WAA) %>%
    dplyr::mutate(WAA_rank = row_number()) %>%
    dplyr::ungroup() %>%
    merge(.data, .)
}
