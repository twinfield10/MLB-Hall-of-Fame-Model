# =============================================================================
# Shiny app — data loading & enrichment (runs once at startup).
#
# Reads the pipeline's prediction CSVs from outputs/predictions/, joins the team
# + primary-position columns the frontend filters need (derived directly from the
# Lahman snapshot's Appearances table), and exposes them as globals the ui/server
# consume. The gt table builders live in R/report_gt.R.
# =============================================================================

suppressMessages({
  library(shiny); library(bslib); library(gt)
  library(dplyr); library(readr); library(tidyr); library(mlbplotR)
})

ROOT <- Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model")
source(file.path(ROOT, "R", "report_gt.R"))

PRED        <- file.path(ROOT, "outputs", "predictions")
SNAP        <- file.path(ROOT, "Data", "snapshots", "lahman_2025")
DATA_SEASON <- 2025L

# ---- team & primary-position maps from the snapshot Appearances table --------
.app <- readr::read_csv(file.path(SNAP, "Appearances.csv"),
                        show_col_types = FALSE, progress = FALSE)

.most_games_team <- function(df) df %>%
  dplyr::transmute(playerID, team = team_abbr(teamID), G = G_all) %>%
  dplyr::group_by(playerID, team) %>%
  dplyr::summarise(G = sum(G, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(playerID) %>%
  dplyr::slice_max(G, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(playerID, team)

career_team_map <- .most_games_team(.app)
season_team_map <- .most_games_team(dplyr::filter(.app, yearID == DATA_SEASON))

pos_map <- .app %>%
  dplyr::group_by(playerID) %>%
  dplyr::summarise(
    P = sum(G_p, na.rm = TRUE), C = sum(G_c, na.rm = TRUE),
    `1B` = sum(G_1b, na.rm = TRUE), `2B` = sum(G_2b, na.rm = TRUE),
    `3B` = sum(G_3b, na.rm = TRUE), SS = sum(G_ss, na.rm = TRUE),
    OF = sum(G_of, na.rm = TRUE), DH = sum(G_dh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(-playerID, names_to = "Pos", values_to = "g") %>%
  dplyr::group_by(playerID) %>%
  dplyr::slice_max(g, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(playerID, Pos = ifelse(g > 0, Pos, NA_character_))

# ---- loaders ----------------------------------------------------------------
.read_pred <- function(f) {
  p <- file.path(PRED, f)
  if (!file.exists(p)) return(NULL)
  suppressWarnings(readr::read_csv(p, show_col_types = FALSE, progress = FALSE))
}

# Future ballots: combine batters + pitchers, attach career team.
load_future <- function(year) {
  d <- dplyr::bind_rows(.read_pred(sprintf("hof_bbwaa_batters_%d.csv", year)),
                        .read_pred(sprintf("hof_bbwaa_pitchers_%d.csv", year)))
  if (!nrow(d)) return(NULL)
  dplyr::left_join(d, career_team_map, by = "playerID")
}

# Historical backtest: one file per cycle, attach career team.
load_hist <- function(year) {
  d <- .read_pred(sprintf("hof_backtest_%d.csv", year))
  if (is.null(d)) return(NULL)
  dplyr::left_join(d, career_team_map, by = "playerID")
}

# Active players: current club (season team, career fallback) + primary position.
enrich_active <- function(d) {
  if (is.null(d)) return(NULL)
  d %>%
    dplyr::left_join(dplyr::rename(season_team_map, .team_s = team), by = "playerID") %>%
    dplyr::left_join(dplyr::rename(career_team_map, .team_c = team), by = "playerID") %>%
    dplyr::mutate(team = dplyr::coalesce(.team_s, .team_c)) %>%
    dplyr::select(-.team_s, -.team_c) %>%
    dplyr::left_join(pos_map, by = "playerID")
}

FUTURE_YEARS <- 2027:2030
FUTURE <- Filter(Negate(is.null), setNames(lapply(FUTURE_YEARS, load_future), FUTURE_YEARS))

hist_files <- list.files(PRED, pattern = "^hof_backtest_[0-9]{4}\\.csv$")
HIST_YEARS <- sort(as.integer(sub("^hof_backtest_([0-9]{4})\\.csv$", "\\1", hist_files)))
HIST <- setNames(lapply(HIST_YEARS, load_hist), HIST_YEARS)

ACTIVE_FROZEN <- enrich_active(.read_pred("active_players_2025.csv"))
ACTIVE_AGING  <- enrich_active(.read_pred("active_hof_projection_2025.csv"))

# ---- filter choices ----------------------------------------------------------
.active_keys <- dplyr::bind_rows(
  if (!is.null(ACTIVE_FROZEN)) dplyr::select(ACTIVE_FROZEN, playerID, team, Pos, Age),
  if (!is.null(ACTIVE_AGING))  dplyr::select(ACTIVE_AGING,  playerID, team, Pos, Age)
)
TEAM_CHOICES <- sort(unique(stats::na.omit(.active_keys$team)))
POS_CHOICES  <- intersect(c("C", "1B", "2B", "3B", "SS", "OF", "DH", "P"),
                          unique(.active_keys$Pos))
AGE_MIN <- suppressWarnings(min(.active_keys$Age, na.rm = TRUE))
AGE_MAX <- suppressWarnings(max(.active_keys$Age, na.rm = TRUE))
