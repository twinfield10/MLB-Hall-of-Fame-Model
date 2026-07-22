# =============================================================================
# Feature assembly — the append_*() chains from the two -2024 runner scripts,
# encoded as functions. Order is preserved verbatim (dependencies between steps
# matter: append_age adds bbrefID from People before append_br_war joins WAR;
# append_hof/append_pos add votedBy/POS before append_jaws groups on them).
#
# Phase 4 will collapse these two into one player_type-parameterised pipeline.
# =============================================================================

if (!exists("get_lahman_batting")) source(file.path("R", "features.R"))

#' Assemble the batter feature frame (per-player-season, pre feature-matrix).
assemble_batters <- function(war, label_max_year = NULL) {
  get_lahman_batting() %>%
    append_age() %>%
    append_br_war(war = war$BattingWAR) %>%
    append_pos() %>%
    append_hof(label_max_year = label_max_year) %>%
    append_ws_wins() %>%
    append_pen_wins() %>%
    append_div_wins() %>%
    append_mvps() %>%
    append_all_star() %>%
    append_ROY() %>%
    append_SS() %>%
    append_GG() %>%
    append_TC() %>%
    append_steroids() %>%
    append_jaws() %>%
    append_HOFM() %>%
    append_C_HOFM()
}

#' Assemble the pitcher feature frame (per-player-season, pre feature-matrix).
assemble_pitchers <- function(war, label_max_year = NULL) {
  (get_lahman_pitching() %>% dplyr::mutate(IPouts = round(IPouts / 3, digits = 2))) %>%
    append_age() %>%
    p_append_br_war(war = war$PitchingWAR) %>%
    append_pos() %>%
    append_hof(label_max_year = label_max_year) %>%
    append_post_pitch() %>%
    append_mvps() %>%
    append_all_star() %>%
    append_cys() %>%
    append_ROY() %>%
    append_PTC() %>%
    append_GG() %>%
    append_steroids() %>%
    append_p_jaws() %>%
    p_append_HOFM() %>%
    p_append_C_HOFM()
}
