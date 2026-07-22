#!/usr/bin/env Rscript
# =============================================================================
# Active-player HOF standing — "where are they, career frozen at the data season?"
#
#   Rscript Scripts/active_players.R          # season/snapshot from config.yml
#
# Scores every active player (>=10 seasons) two ways with careers FROZEN as-is
# (no projection yet): merit P(HOF) from the any-route model, and a BBWAA
# "if retired today" projected ballot share + election probabilities. Writes
# outputs/predictions/active_players_<data_season>.csv and a gt chart. See
# R/active_players.R. Career projection is the next phase.
# =============================================================================

setwd(Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model"))

.R_FILES <- c("data_snapshot", "config", "data_supplement", "tables", "data_live",
              "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
              "model", "evaluate", "committee_bbwaa", "pipeline", "active_players")
suppressMessages(for (f in .R_FILES) source(file.path("R", paste0(f, ".R"))))

args   <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[1]) else NULL
cfg <- load_config("config.yml", season = season)
message(sprintf("Active players | data_season %d | snapshot %s", cfg$data_season, cfg$snapshot))

init_tables(cfg)
war     <- load_war(cfg)
cohorts <- build_cohorts(cfg)

report <- build_active_report(cfg, war, cohorts, min_seasons = 10)

dir <- file.path(cfg$paths$outputs, "predictions")
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
out_csv <- file.path(dir, sprintf("active_players_%d.csv", cfg$data_season))
utils::write.csv(report, out_csv, row.names = FALSE)
message(sprintf("wrote %s (%d active players, 10+ seasons)", out_csv, nrow(report)))

# ---- chart (best-effort; needs gt + mlbplotR) -------------------------------
render_chart <- function() {
  suppressMessages({ library(gt); library(mlbplotR); library(dplyr); library(scales) })

  team_abbr <- function(x) dplyr::case_when(
    x=="NYA"~"NYY", x=="SLN"~"STL", x=="KCA"~"KC", x=="CHN"~"CHC", x=="ARI"~"AZ",
    x=="SDN"~"SD", x=="TBA"~"TB", x=="ANA"~"LAA", x=="FLO"~"MIA", x=="CHA"~"CWS",
    x=="NYN"~"NYM", x=="SFN"~"SF", x=="WAS"~"WSH", x=="LAN"~"LAD", x=="MON"~"WSH", x=="TBD"~"TB", TRUE~x)
  primary_team <- function(tblname) tbl(tblname) %>%
    dplyr::select(playerID, teamID, G) %>% dplyr::mutate(teamID = team_abbr(teamID)) %>%
    dplyr::group_by(playerID, teamID) %>% dplyr::summarise(G = sum(G, na.rm=TRUE), .groups="drop") %>%
    dplyr::group_by(playerID) %>% dplyr::slice_max(G, n=1, with_ties=FALSE) %>% dplyr::ungroup() %>%
    dplyr::transmute(playerID, team = teamID)
  teams <- dplyr::bind_rows(primary_team("Batting"), primary_team("Pitching")) %>%
    dplyr::distinct(playerID, .keep_all = TRUE)

  top <- report %>%
    dplyr::left_join(teams, by = "playerID") %>%
    dplyr::filter(P_HOF >= 0.01 | proj_share_ifretired >= 0.05) %>%
    dplyr::transmute(
      Name = paste(nameFirst, nameLast), team, Type = type, Age, Szn = Seasons,
      Career = CareerWAR, PerYr = WARperSeason, Peak = PeakWAR, JAWS, Monitor = HOFM,
      P_HOF, Proj = proj_share_ifretired, Elect = P_elected_ifretired, Ever = P_eventual_ifretired
    )
  .green <- c("#f7f7f7", "#d6efd6", "#98e6bb", "#46d285", "#1e7b48")
  gt_obj <- top %>% gt() %>%
    cols_label(Name=md("Player"), team=md("Team"), Type=md("Type"), Age=md("Age"), Szn=md("Szn"),
               Career=md("Career"), PerYr=md("Per Yr"), Peak=md("Peak Szn"), JAWS=md("JAWS"),
               Monitor=md("Monitor"), P_HOF=md("P(HOF)"), Proj=md("Proj. Share"),
               Elect=md("This Cycle"), Ever=md("Eventually")) %>%
    tab_spanner(label="Player", columns=c(Name, team, Type, Age, Szn), id="PLY") %>%
    tab_spanner(label="WAR", columns=c(Career, PerYr, Peak), id="WAR") %>%
    tab_spanner(label="HOF Metrics", columns=c(JAWS, Monitor), id="MET") %>%
    tab_spanner(label="Merit Model", columns=c(P_HOF), id="MERIT") %>%
    tab_spanner(label="If Retired Today (BBWAA)", columns=c(Proj, Elect, Ever), id="IFR") %>%
    tab_style(style=list(cell_fill(color="#2F4985"), cell_text(weight="bold", style="italic", color="white"),
                         cell_borders(sides=c("right","left","bottom"), color="#BBBBBB", weight=px(2))),
              locations=cells_column_spanners(spanners=c("PLY","WAR","MET","MERIT","IFR"))) %>%
    tab_style(locations=cells_column_labels(), style=cell_text(weight="bold", align="center")) %>%
    tab_style(locations=cells_body(columns=Name), style=cell_text(align="left")) %>%
    gt_fmt_mlb_scoreboard_logo(columns="team") %>%
    data_color(columns=c(P_HOF, Proj, Elect, Ever), palette=.green, domain=c(0,1), na_color="#f7f7f7") %>%
    data_color(columns=Career, palette=.green) %>% data_color(columns=PerYr, palette=.green) %>%
    data_color(columns=Peak, palette=.green) %>% data_color(columns=JAWS, palette=.green) %>%
    data_color(columns=Monitor, palette=.green) %>%
    fmt_percent(columns=c(P_HOF, Proj, Elect, Ever), decimals=1) %>%
    fmt_number(columns=c(Career, Peak, JAWS), decimals=1) %>%
    fmt_number(columns=PerYr, decimals=2) %>% fmt_number(columns=Monitor, decimals=0) %>%
    cols_align("center", columns=c(team, Type, Age, Szn, Career, PerYr, Peak, JAWS, Monitor, P_HOF, Proj, Elect, Ever)) %>%
    tab_header(title=md("<span style='color:white'>**Active Players — HOF Standing**</span>"),
               subtitle=md(sprintf("<span style='color:white'>*Careers frozen at %d (no projection). Merit P(HOF) + 'if retired today' BBWAA ballot projection.*</span>", cfg$data_season))) %>%
    tab_source_note(md(sprintf("_Merit model = any-route P(inducted). 'If retired today' = committee vote model, scored as a first-ballot candidate on a typical modern field. Career-frozen totals under-rate mid-career players — projection is the next phase. Data through %d._", cfg$data_season))) %>%
    tab_options(heading.background.color="#001C5D", data_row.padding=px(4),
                column_labels.border.bottom.color="black", column_labels.border.bottom.width=px(2))

  rdir <- file.path(cfg$paths$outputs, "reports")
  if (!dir.exists(rdir)) dir.create(rdir, recursive=TRUE)
  out_html <- file.path(rdir, sprintf("active_players_%d.html", cfg$data_season))
  gtsave(gt_obj, out_html)
  message(sprintf("wrote %s (%d players shown)", out_html, nrow(top)))
}
chart <- try(render_chart(), silent = TRUE)
if (inherits(chart, "try-error"))
  message("   (chart skipped — gt/mlbplotR unavailable; CSV written)")
message("Done.")
