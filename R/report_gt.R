# =============================================================================
# Shared "Cooperstown Likelihood" gt styling.
#
# The single source of truth for the HOF report look — navy header (#001C5D),
# blue spanners (#2F4985), the green sequential scale, gold highlight for
# candidates clearing the 75% election line, 25%-opacity red for those below the
# 5% survival line, MLB team logos, and the PED asterisk. Extracted from
# Scripts/pretty_bbwaa_ballot.R so the CLI report scripts AND the Shiny app render
# identical tables. Builders take the RAW prediction schema (the CSVs written by
# the pipeline) plus a joined `team` column and do all shaping + styling.
# =============================================================================

suppressMessages({ library(dplyr); library(gt); library(mlbplotR); library(scales) })

# ---- design tokens ----------------------------------------------------------
.HOF_GREEN <- c("#f7f7f7", "#d6efd6", "#98e6bb", "#46d285", "#1e7b48")
.HOF_NAVY  <- "#001C5D"
.HOF_BLUE  <- "#2F4985"
.HOF_GOLD  <- "#E7C548"

# ---- helpers (moved from Scripts/pretty_bbwaa_ballot.R) ----------------------

#' Lahman team codes -> modern abbreviations mlbplotR understands.
team_abbr <- function(x) dplyr::case_when(
  x == "NYA" ~ "NYY", x == "SLN" ~ "STL", x == "KCA" ~ "KC",  x == "CHN" ~ "CHC",
  x == "ARI" ~ "AZ",  x == "SDN" ~ "SD",  x == "TBA" ~ "TB",  x == "ANA" ~ "LAA",
  x == "FLO" ~ "MIA", x == "CHA" ~ "CWS", x == "NYN" ~ "NYM", x == "SFN" ~ "SF",
  x == "WAS" ~ "WSH", x == "LAN" ~ "LAD", x == "MON" ~ "WSH", x == "TBD" ~ "TB",
  x == "CAL" ~ "LAA", x == "ML4" ~ "MIL", x == "WS2" ~ "MIN",
  TRUE ~ x
)

#' Primary (most-games) team per player across their whole career, from the
#' appropriate table for the player type. Returns playerID -> team (abbr).
primary_team <- function(table_name = "Appearances") {
  g_col <- if (table_name == "Appearances") "G_all" else "G"
  tbl(table_name) %>%
    dplyr::transmute(playerID, teamID = team_abbr(teamID), G = .data[[g_col]]) %>%
    dplyr::group_by(playerID, teamID) %>%
    dplyr::summarise(G = sum(G, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(playerID) %>%
    dplyr::slice_max(G, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(playerID, team = teamID)
}

#' Team the player logged the most games for in a given season (their current
#' club, for active players). Returns playerID -> team (abbr).
season_team <- function(season, table_name = "Appearances") {
  g_col <- if (table_name == "Appearances") "G_all" else "G"
  tbl(table_name) %>%
    dplyr::filter(yearID == season) %>%
    dplyr::transmute(playerID, teamID = team_abbr(teamID), G = .data[[g_col]]) %>%
    dplyr::group_by(playerID, teamID) %>%
    dplyr::summarise(G = sum(G, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(playerID) %>%
    dplyr::slice_max(G, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(playerID, team = teamID)
}

#' Primary defensive position per player, argmax of per-position games in
#' Appearances (outfield spots collapsed to OF). Returns playerID -> pos.
primary_position <- function(season = NULL) {
  ap <- tbl("Appearances")
  if (!is.null(season)) ap <- dplyr::filter(ap, yearID == season)
  ap %>%
    dplyr::group_by(playerID) %>%
    dplyr::summarise(
      P  = sum(G_p,  na.rm = TRUE), C  = sum(G_c,  na.rm = TRUE),
      `1B` = sum(G_1b, na.rm = TRUE), `2B` = sum(G_2b, na.rm = TRUE),
      `3B` = sum(G_3b, na.rm = TRUE), SS = sum(G_ss, na.rm = TRUE),
      OF = sum(G_of, na.rm = TRUE), DH = sum(G_dh, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-playerID, names_to = "pos", values_to = "g") %>%
    dplyr::group_by(playerID) %>%
    dplyr::slice_max(g, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(playerID, pos = ifelse(g > 0, pos, NA_character_))
}

#' Append a PED marker to a player name (red asterisk).
ped_mark <- function(name, ped) {
  flagged <- !is.na(ped) & ped == 1
  ifelse(flagged, paste0(name, "<span style='color:#c0392b;font-weight:bold;'> *</span>"), name)
}

#' Proper ordinal suffix for ballot appearance number (1st, 2nd, 3rd, 4th...).
ord_suffix <- function(n) {
  s <- rep("th", length(n))
  s[n %% 10 == 1 & n %% 100 != 11] <- "st"
  s[n %% 10 == 2 & n %% 100 != 12] <- "nd"
  s[n %% 10 == 3 & n %% 100 != 13] <- "rd"
  s
}

#' Common table-wide options: navy heading, grey outer borders, tight rows.
.cooperstown_options <- function(g) {
  g %>% tab_options(
    table.border.top.color = "#BBBBBB", table.border.top.width = px(3),
    table.border.bottom.color = "#BBBBBB", table.border.bottom.width = px(3),
    heading.background.color = .HOF_NAVY,
    heading.title.font.size = px(22), heading.subtitle.font.size = px(14),
    column_labels.border.bottom.color = "black", column_labels.border.bottom.width = px(2),
    data_row.padding = px(4), table.font.size = px(15)
  )
}

#' Style the given spanner ids navy/blue with white bold-italic text, and bold
#' the column labels.
.style_spanners <- function(g, ids) {
  g %>%
    tab_style(
      style = list(cell_fill(color = .HOF_BLUE),
                   cell_text(weight = "bold", style = "italic", color = "white"),
                   cell_borders(sides = c("right", "left", "bottom"), color = "#BBBBBB", weight = px(2))),
      locations = cells_column_spanners(spanners = ids)
    ) %>%
    tab_style(locations = cells_column_labels(), style = cell_text(weight = "bold", align = "center"))
}

.empty_gt <- function(msg = "No players match the current filters.") {
  gt(data.frame(` ` = msg, check.names = FALSE)) %>% .cooperstown_options()
}

# ---- ballot table (future projections + historical predicted-vs-actual) -----

#' Render a BBWAA ballot as a Cooperstown gt table.
#'
#' @param raw         a prediction data.frame (hof_bbwaa_* or hof_backtest_*
#'                    schema) with a joined `team` column.
#' @param season      cycle year (labels the projected-share column).
#' @param historical  FALSE = future projection layout; TRUE = predicted-vs-actual.
#' @param elect_pct,survive_pct  gold / red thresholds.
build_ballot_gt <- function(raw, season, historical = FALSE,
                            elect_pct = 0.75, survive_pct = 0.05,
                            title = NULL, subtitle = NULL, source_note = NULL) {
  if (is.null(raw) || !nrow(raw)) return(.empty_gt("No candidates on this ballot."))

  d <- raw %>% dplyr::arrange(dplyr::desc(pred_share), dplyr::desc(P_eventual_bbwaa))
  took <- if ("TookSteroids" %in% names(d)) d$TookSteroids else rep(NA_real_, nrow(d))

  disp <- dplyr::tibble(
    Name    = ped_mark(paste(d$nameFirst, d$nameLast), took),
    team    = d$team,
    POS     = d$POS,
    Yr      = ifelse(d$is_first_ballot == 1, "1st",
                     paste0(d$appearance_number, ord_suffix(d$appearance_number))),
    Career  = d$CareerWAR, PerYr = d$WARperSeason, Peak = d$PeakWAR,
    JAWS    = d$JAWS, Monitor = d$HOFM,
    Proj    = d$pred_share,
    Elect   = d$P_elected_cycle
  )
  if (historical) {
    disp$Actual <- d$actual_share
    disp$Result <- ifelse(!is.na(d$actual_inducted) & d$actual_inducted, "Elected", "—")
  } else {
    disp$Prev <- ifelse(d$is_first_ballot == 1, NA_real_, d$prev_share)
    disp$Ever <- d$P_eventual_bbwaa
  }

  had_ped <- any(grepl("\\*", disp$Name))
  gold_rows <- which(disp$Proj >= elect_pct)
  red_rows  <- which(disp$Proj <  survive_pct)

  g <- gt(disp) %>%
    fmt_markdown(columns = Name) %>%
    tab_spanner(label = "Player",      columns = c(Name, team, POS, Yr), id = "PLY") %>%
    tab_spanner(label = "WAR",         columns = c(Career, PerYr, Peak), id = "WAR") %>%
    tab_spanner(label = "HOF Metrics", columns = c(JAWS, Monitor),       id = "MET")

  if (historical) {
    g <- g %>%
      cols_label(Name = "Player", team = "Team", POS = "Pos.", Yr = "Ballot",
                 Career = "Career", PerYr = "Per Yr", Peak = "Peak Szn",
                 JAWS = "JAWS", Monitor = "Monitor",
                 Proj = "Proj. Share", Elect = "P(Elect)",
                 Actual = "Actual Share", Result = "Result") %>%
      tab_spanner(label = "Model Prediction", columns = c(Proj, Elect),   id = "MODEL") %>%
      tab_spanner(label = "Actual Result",    columns = c(Actual, Result), id = "ACT") %>%
      .style_spanners(c("PLY", "WAR", "MET", "MODEL", "ACT")) %>%
      data_color(columns = c(Proj, Elect, Actual), palette = .HOF_GREEN,
                 domain = c(0, 1), na_color = "#f7f7f7") %>%
      fmt_percent(columns = c(Proj, Elect, Actual), decimals = 1) %>%
      cols_align("center", columns = c(team, POS, Yr, Career, PerYr, Peak, JAWS,
                                       Monitor, Proj, Elect, Actual, Result))
    elected_rows <- which(disp$Result == "Elected")
    if (length(elected_rows))
      g <- g %>% tab_style(locations = cells_body(columns = Result, rows = elected_rows),
                           style = list(cell_fill(color = .HOF_GOLD), cell_text(weight = "bold")))
  } else {
    g <- g %>%
      cols_label(Name = "Player", team = "Team", POS = "Pos.", Yr = "Ballot",
                 Career = "Career", PerYr = "Per Yr", Peak = "Peak Szn",
                 JAWS = "JAWS", Monitor = "Monitor",
                 Prev = as.character(season - 1), Proj = paste("Proj.", season),
                 Elect = "This Cycle", Ever = "Eventually") %>%
      tab_spanner(label = "Vote Share",          columns = c(Prev, Proj),  id = "SHARE") %>%
      tab_spanner(label = "P(Elected by BBWAA)", columns = c(Elect, Ever), id = "PROB") %>%
      .style_spanners(c("PLY", "WAR", "MET", "SHARE", "PROB")) %>%
      data_color(columns = c(Prev, Proj, Elect, Ever), palette = .HOF_GREEN,
                 domain = c(0, 1), na_color = "#f7f7f7") %>%
      fmt_percent(columns = c(Prev, Proj, Elect, Ever), decimals = 1) %>%
      sub_missing(columns = Prev, missing_text = "—") %>%
      cols_align("center", columns = c(team, POS, Yr, Career, PerYr, Peak, JAWS,
                                       Monitor, Prev, Proj, Elect, Ever))
  }

  # WAR / metric columns shaded within their own range.
  for (col in c("Career", "PerYr", "Peak", "JAWS", "Monitor"))
    g <- g %>% data_color(columns = all_of(col), palette = .HOF_GREEN, na_color = "#f7f7f7")

  g <- g %>%
    tab_style(locations = cells_body(columns = Name), style = cell_text(align = "left")) %>%
    gt_fmt_mlb_scoreboard_logo(columns = "team") %>%
    fmt_number(columns = c(Career, Peak, JAWS), decimals = 1) %>%
    fmt_number(columns = PerYr, decimals = 2) %>%
    fmt_number(columns = Monitor, decimals = 0)

  if (length(gold_rows))
    g <- g %>% tab_style(locations = cells_body(columns = Proj, rows = gold_rows),
                         style = list(cell_fill(color = .HOF_GOLD), cell_text(weight = "bold")))
  if (length(red_rows))
    g <- g %>% tab_style(locations = cells_body(columns = Proj, rows = red_rows),
                         style = cell_fill(color = "red", alpha = 0.25))

  title <- title %||% "Cooperstown Likelihood"
  subtitle <- subtitle %||% sprintf(
    "%d BBWAA Hall of Fame Ballot — %s", season,
    if (historical) "model prediction vs. actual result" else "projected vote share & election probability")
  g %>%
    tab_header(title = md(sprintf("<span style='color:white'>**%s**</span>", title)),
               subtitle = md(sprintf("<span style='color:white'>*%s*</span>", subtitle))) %>%
    tab_source_note(md(source_note %||% sprintf(
      "_Election requires **75%%** of the vote (gold); **<5%%** drops off the ballot next year (red).%s Model: BBWAA share regressor + forward simulation._",
      if (had_ped) " <span style='color:#c0392b;font-weight:bold;'>*</span> linked to PEDs." else ""))) %>%
    .cooperstown_options()
}

# ---- active-player tables ---------------------------------------------------

#' Render active players as a Cooperstown gt table.
#'
#' @param raw   active_players_* (kind="frozen") or active_hof_projection_*
#'              (kind="aging") schema, with joined `team` + `Pos` columns.
#' @param kind  "frozen" = careers frozen as-is; "aging" = aging-curve projection.
build_active_gt <- function(raw, kind = c("frozen", "aging"),
                            title = NULL, subtitle = NULL, source_note = NULL) {
  kind <- match.arg(kind)
  if (is.null(raw) || !nrow(raw)) return(.empty_gt())

  if (kind == "frozen") {
    d <- raw %>% dplyr::arrange(dplyr::desc(P_HOF))
    disp <- dplyr::tibble(
      Name = paste(d$nameFirst, d$nameLast), team = d$team, Pos = d$Pos, Age = d$Age,
      Career = d$CareerWAR, PerYr = d$WARperSeason, Peak = d$PeakWAR,
      JAWS = d$JAWS, Monitor = d$HOFM,
      PHOF = d$P_HOF,
      Share = d$proj_share_ifretired, Cyc = d$P_elected_ifretired, Ever = d$P_eventual_ifretired
    )
    g <- gt(disp) %>%
      tab_spanner(label = "Player",       columns = c(Name, team, Pos, Age), id = "PLY") %>%
      tab_spanner(label = "WAR",          columns = c(Career, PerYr, Peak),  id = "WAR") %>%
      tab_spanner(label = "HOF Metrics",  columns = c(JAWS, Monitor),        id = "MET") %>%
      tab_spanner(label = "If Retired Today (BBWAA)", columns = c(Share, Cyc, Ever), id = "RET") %>%
      .style_spanners(c("PLY", "WAR", "MET", "RET")) %>%
      cols_label(Name = "Player", team = "Team", Pos = "Pos.", Age = "Age",
                 Career = "Career", PerYr = "Per Yr", Peak = "Peak Szn",
                 JAWS = "JAWS", Monitor = "Monitor",
                 PHOF = md("P(HOF)"), Share = "Proj. Share", Cyc = "This Cycle", Ever = "Eventually") %>%
      data_color(columns = c(PHOF, Share, Cyc, Ever), palette = .HOF_GREEN,
                 domain = c(0, 1), na_color = "#f7f7f7") %>%
      fmt_percent(columns = c(PHOF, Share, Cyc, Ever), decimals = 1) %>%
      cols_align("center", columns = c(team, Pos, Age, Career, PerYr, Peak, JAWS,
                                       Monitor, PHOF, Share, Cyc, Ever))
    shade <- c("Career", "PerYr", "Peak", "JAWS", "Monitor")
    for (col in shade) g <- g %>% data_color(columns = all_of(col), palette = .HOF_GREEN, na_color = "#f7f7f7")
    g <- g %>%
      fmt_number(columns = c(Career, Peak, JAWS), decimals = 1) %>%
      fmt_number(columns = PerYr, decimals = 2) %>%
      fmt_number(columns = Monitor, decimals = 0)
    title <- title %||% "Active Players — If Career Ended Today"
    subtitle <- subtitle %||% "HOF merit (any route) and projected BBWAA outcome if the player retired now"
    src <- source_note %||% "_P(HOF) = merit model. “If Retired Today” scores the player as a first-ballot candidate against a typical modern field._"
  } else {
    d <- raw %>% dplyr::arrange(dplyr::desc(P_HOF))
    disp <- dplyr::tibble(
      Name = d$name, team = d$team, Pos = d$Pos, Age = d$Age,
      Seasons = d$real_seasons, RealWAR = d$real_WAR, PFrozen = d$P_HOF_frozen,
      AddYrs = d$added_years, ProjWAR = d$proj_WAR_med, ProjLo = d$proj_WAR_p10, ProjHi = d$proj_WAR_p90,
      PProj = d$P_HOF
    )
    g <- gt(disp) %>%
      tab_spanner(label = "Player",             columns = c(Name, team, Pos, Age),       id = "PLY") %>%
      tab_spanner(label = "Career To Date",     columns = c(Seasons, RealWAR, PFrozen),   id = "NOW") %>%
      tab_spanner(label = "Aging-Curve Projection", columns = c(AddYrs, ProjWAR, ProjLo, ProjHi, PProj), id = "PRJ") %>%
      .style_spanners(c("PLY", "NOW", "PRJ")) %>%
      cols_label(Name = "Player", team = "Team", Pos = "Pos.", Age = "Age",
                 Seasons = "Seasons", RealWAR = "WAR", PFrozen = md("P(HOF)"),
                 AddYrs = "+Yrs", ProjWAR = "Proj. WAR", ProjLo = "p10", ProjHi = "p90", PProj = md("P(HOF)")) %>%
      data_color(columns = c(PFrozen, PProj), palette = .HOF_GREEN, domain = c(0, 1), na_color = "#f7f7f7") %>%
      fmt_percent(columns = c(PFrozen, PProj), decimals = 1) %>%
      fmt_number(columns = c(RealWAR, ProjWAR, ProjLo, ProjHi), decimals = 1) %>%
      cols_align("center", columns = c(team, Pos, Age, Seasons, RealWAR, PFrozen,
                                       AddYrs, ProjWAR, ProjLo, ProjHi, PProj))
    for (col in c("RealWAR", "ProjWAR")) g <- g %>% data_color(columns = all_of(col), palette = .HOF_GREEN, na_color = "#f7f7f7")
    title <- title %||% "Active Players — With Aging-Curve Projection"
    subtitle <- subtitle %||% "Career projected forward with age curves + retirement hazard (Monte-Carlo)"
    src <- source_note %||% "_Projected WAR is the median path; p10–p90 is the simulation band. P(HOF) reflects the full projected résumé._"
  }

  g %>%
    tab_style(locations = cells_body(columns = Name), style = cell_text(align = "left")) %>%
    gt_fmt_mlb_scoreboard_logo(columns = "team") %>%
    tab_header(title = md(sprintf("<span style='color:white'>**%s**</span>", title)),
               subtitle = md(sprintf("<span style='color:white'>*%s*</span>", subtitle))) %>%
    tab_source_note(md(src)) %>%
    .cooperstown_options()
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
