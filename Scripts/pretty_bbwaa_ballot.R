#!/usr/bin/env Rscript
# =============================================================================
# Pretty 2026 BBWAA ballot report.
#
# Reads the committee model's predictions (outputs/predictions/hof_bbwaa_*.csv),
# combines batters + pitchers, and renders a styled `gt` "Cooperstown Likelihood"
# table (navy header, blue spanners, MLB team logos, WAR + HOF-metric columns on
# white->green scales, green probability scale, gold highlight for candidates
# projected to clear 75%) to HTML — the modern successor to PrettyHOFCharts.R,
# driven by the persisted predictions rather than in-memory globals.
#
#   Rscript Scripts/pretty_bbwaa_ballot.R          # season from config.yml
#   Rscript Scripts/pretty_bbwaa_ballot.R 2027
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(gt); library(mlbplotR); library(scales)
})

setwd(Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model"))
for (f in c("data_snapshot", "config", "data_supplement", "tables"))
  suppressWarnings(suppressMessages(source(file.path("R", paste0(f, ".R")))))

args   <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[1]) else NULL
# Reporting only reads already-written predictions + reference tables (team logos),
# so the snapshot-vintage guard doesn't apply — relax max_data_lag so charts for
# future PROJECTED cycles (2028+, beyond the stats horizon) still render.
cfg <- load_config("config.yml", season = season,
                   overrides = list(max_data_lag = 1000L)); init_tables(cfg)

# Lahman team codes -> modern abbreviations mlbplotR understands (from PrettyHOFCharts.R).
team_abbr <- function(x) dplyr::case_when(
  x == "NYA" ~ "NYY", x == "SLN" ~ "STL", x == "KCA" ~ "KC",  x == "CHN" ~ "CHC",
  x == "ARI" ~ "AZ",  x == "SDN" ~ "SD",  x == "TBA" ~ "TB",  x == "ANA" ~ "LAA",
  x == "FLO" ~ "MIA", x == "CHA" ~ "CWS", x == "NYN" ~ "NYM", x == "SFN" ~ "SF",
  x == "WAS" ~ "WSH", x == "LAN" ~ "LAD", x == "MON" ~ "WSH", x == "TBD" ~ "TB",
  TRUE ~ x
)

# Primary team = most games played, from the appropriate table for the player type.
primary_team <- function(table_name) {
  tbl(table_name) %>%
    dplyr::select(playerID, teamID, G) %>%
    dplyr::mutate(teamID = team_abbr(teamID)) %>%
    dplyr::group_by(playerID, teamID) %>%
    dplyr::summarise(G = sum(G, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(playerID) %>%
    dplyr::slice_max(G, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(playerID, team = teamID)
}

read_preds <- function(type, team_table) {
  path <- file.path(cfg$paths$outputs, "predictions",
                    sprintf("hof_bbwaa_%ss_%d.csv", type, cfg$season))
  suppressWarnings(read_csv(path, show_col_types = FALSE)) %>%
    dplyr::left_join(primary_team(team_table), by = "playerID")
}

# ---- steroid marker: how PED-linked players are flagged in the Player column ----
# swap PED_STYLE among "asterisk" | "syringe" | "badge"
PED_STYLE <- "asterisk"
ped_mark <- function(name, ped) {
  flagged <- !is.na(ped) & ped == 1
  if (PED_STYLE == "syringe") {
    ifelse(flagged, paste0(name, " \U1F489"), name)
  } else if (PED_STYLE == "badge") {
    ifelse(flagged,
           paste0(name, " <span style='background:#c0392b;color:white;font-size:9px;",
                  "font-weight:bold;padding:1px 4px;border-radius:3px;vertical-align:middle;'>PED</span>"),
           name)
  } else { # asterisk
    ifelse(flagged, paste0(name, "<span style='color:#c0392b;font-weight:bold;'> *</span>"), name)
  }
}

ballot <- dplyr::bind_rows(read_preds("batter", "Batting"),
                           read_preds("pitcher", "Pitching")) %>%
  dplyr::arrange(dplyr::desc(pred_share), dplyr::desc(P_eventual_bbwaa)) %>%
  dplyr::transmute(
    Name  = ped_mark(paste(nameFirst, nameLast), TookSteroids),
    team, POS,
    Yr    = ifelse(is_first_ballot == 1, "1st", paste0(appearance_number, "th")),
    Career = CareerWAR, PerYr = WARperSeason, Peak = PeakWAR,
    JAWS, Monitor = HOFM,
    Prev  = ifelse(is_first_ballot == 1, NA_real_, prev_share),   # no prior share for first-timers
    Proj  = pred_share,
    Elect = P_elected_cycle,
    Ever  = P_eventual_bbwaa
  )

.green <- c("#f7f7f7", "#d6efd6", "#98e6bb", "#46d285", "#1e7b48")
had_ped <- any(grepl("PED|\U1F489|\\*", ballot$Name))

gt_obj <- ballot %>%
  gt() %>%
  fmt_markdown(columns = Name) %>%
  cols_label(Name = md("Player"), team = md("Team"), POS = md("Pos."), Yr = md("Ballot"),
             Career = md("Career"), PerYr = md("Per Yr"), Peak = md("Peak Szn"),
             JAWS = md("JAWS"), Monitor = md("Monitor"),
             Prev = md(sprintf("%d", cfg$season - 1)), Proj = md(sprintf("Proj. %d", cfg$season)),
             Elect = md("This Cycle"), Ever = md("Eventually")) %>%
  tab_spanner(label = "Player",              columns = c(Name, team, POS, Yr), id = "PLY") %>%
  tab_spanner(label = "WAR",                 columns = c(Career, PerYr, Peak), id = "WAR") %>%
  tab_spanner(label = "HOF Metrics",         columns = c(JAWS, Monitor),       id = "MET") %>%
  tab_spanner(label = "Vote Share",          columns = c(Prev, Proj),          id = "SHARE") %>%
  tab_spanner(label = "P(Elected by BBWAA)", columns = c(Elect, Ever),         id = "PROB") %>%
  # spanner styling (navy/blue, white italic — matches the legacy look)
  tab_style(
    style = list(cell_fill(color = "#2F4985"),
                 cell_text(weight = "bold", style = "italic", color = "white"),
                 cell_borders(sides = c("right", "left", "bottom"), color = "#BBBBBB", weight = px(2))),
    locations = cells_column_spanners(spanners = c("PLY", "WAR", "MET", "SHARE", "PROB"))
  ) %>%
  tab_style(locations = cells_column_labels(),  style = cell_text(weight = "bold", align = "center")) %>%
  tab_style(locations = cells_body(columns = Name), style = cell_text(align = "left")) %>%
  gt_fmt_mlb_scoreboard_logo(columns = "team") %>%
  # white->green scales — probabilities on a fixed 0-1 domain, WAR/metrics each on their own range
  data_color(columns = c(Prev, Proj, Elect, Ever), palette = .green, domain = c(0, 1), na_color = "#f7f7f7") %>%
  data_color(columns = Career,  palette = .green) %>%
  data_color(columns = PerYr,   palette = .green) %>%
  data_color(columns = Peak,    palette = .green) %>%
  data_color(columns = JAWS,    palette = .green) %>%
  data_color(columns = Monitor, palette = .green) %>%
  # gold highlight for anyone projected to clear the 75% election line
  tab_style(
    locations = cells_body(columns = Proj, rows = Proj >= cfg$bbwaa$elect_pct),
    style = list(cell_fill(color = "#E7C548"), cell_text(weight = "bold"))
  ) %>%
  # 25%-opacity red for anyone projected below the 5% survival line (drops off next year)
  tab_style(
    locations = cells_body(columns = Proj, rows = Proj < cfg$bbwaa$survive_pct),
    style = cell_fill(color = "red", alpha = 0.25)
  ) %>%
  fmt_percent(columns = c(Prev, Proj, Elect, Ever), decimals = 1) %>%
  fmt_number(columns = c(Career, Peak, JAWS), decimals = 1) %>%
  fmt_number(columns = PerYr, decimals = 2) %>%
  fmt_number(columns = Monitor, decimals = 0) %>%
  sub_missing(columns = Prev, missing_text = "—") %>%
  cols_align("center", columns = c(team, POS, Yr, Career, PerYr, Peak, JAWS, Monitor,
                                   Prev, Proj, Elect, Ever)) %>%
  tab_header(
    title    = md("<span style='color:white'>**Cooperstown Likelihood**</span>"),
    subtitle = md(sprintf("<span style='color:white'>*%d BBWAA Hall of Fame Ballot — projected vote share & election probability*</span>", cfg$season))
  ) %>%
  tab_source_note(md(sprintf(
    "_Election requires **75%%** of the vote (gold); **<5%%** drops off the ballot next year (red). Ballot tenure caps at 10 years.%s WAR/JAWS/Monitor shaded within-column. Model: committee-specific BBWAA share regressor + forward simulation · data through %d._",
    if (had_ped) " <span style='color:#c0392b;font-weight:bold;'>*</span> linked to PEDs." else "", cfg$data_season))) %>%
  tab_options(
    table.border.top.color = "#BBBBBB", table.border.top.width = px(3),
    table.border.bottom.color = "#BBBBBB", table.border.bottom.width = px(3),
    heading.background.color = "#001C5D",
    column_labels.border.bottom.color = "black", column_labels.border.bottom.width = px(2),
    data_row.padding = px(4)
  )

dir <- file.path(cfg$paths$outputs, "reports")
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
out <- file.path(dir, sprintf("hof_bbwaa_ballot_%d.html", cfg$season))
gtsave(gt_obj, out)
message(sprintf("wrote %s (%d candidates, PED style: %s)", out, nrow(ballot), PED_STYLE))

# Open in the default browser.
try(utils::browseURL(normalizePath(out)), silent = TRUE)
