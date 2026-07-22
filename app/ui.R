# =============================================================================
# Shiny app — UI. Three tabs: Future Ballots (landing, 2027), Historical
# Ballots (predicted vs actual), Active Players (two filterable tables).
# =============================================================================

.app_css <- "
  .navbar { background-color: #001C5D !important; }
  .navbar .navbar-brand, .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { color: #E7C548 !important; font-weight: 600; }
  body { background-color: #f4f5f7; }
  .gt_table_wrap { overflow-x: auto; padding: 6px 2px 18px; }
  h4.section { color: #001C5D; font-weight: 700; margin: 8px 0 2px; }
  .lede { color: #5a6270; font-size: 0.9rem; margin-bottom: 10px; }
"

ui <- page_navbar(
  title  = "MLB Hall of Fame Model",
  id     = "main_nav",
  theme  = bs_theme(version = 5, primary = "#001C5D", base_font = font_google("Inter")),
  header = tags$head(tags$style(HTML(.app_css))),
  selected = "Future Ballots",

  nav_panel(
    "Future Ballots",
    layout_sidebar(
      sidebar = sidebar(
        title = "Ballot",
        selectInput("future_year", "Cycle year",
                    choices = as.integer(names(FUTURE)),
                    selected = if ("2027" %in% names(FUTURE)) 2027L else as.integer(names(FUTURE))[1]),
        p(class = "lede", "Projected BBWAA ballots for cycles not yet held. Gold = projected to clear 75%; red = below the 5% survival line.")
      ),
      div(class = "gt_table_wrap", gt_output("future_tbl"))
    )
  ),

  nav_panel(
    "Historical Ballots",
    layout_sidebar(
      sidebar = sidebar(
        title = "Ballot",
        selectInput("hist_year", "Cycle year",
                    choices = rev(HIST_YEARS),
                    selected = if (length(HIST_YEARS)) max(HIST_YEARS) else NULL),
        p(class = "lede", "How the model would have called each past ballot, trained only on prior cycles (no leakage), next to the actual result.")
      ),
      div(class = "gt_table_wrap", gt_output("hist_tbl"))
    )
  ),

  nav_panel(
    "Active Players",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        selectInput("team_f", "Team", choices = TEAM_CHOICES, multiple = TRUE),
        selectInput("pos_f", "Position", choices = POS_CHOICES, multiple = TRUE),
        sliderInput("age_f", "Age range",
                    min = AGE_MIN, max = AGE_MAX, value = c(AGE_MIN, AGE_MAX), step = 1),
        p(class = "lede", "Current players' Hall of Fame odds — first with careers frozen as-is, then projected forward with aging curves.")
      ),
      h4(class = "section", "If Career Ended Today"),
      div(class = "gt_table_wrap", gt_output("active_frozen")),
      tags$hr(),
      h4(class = "section", "With Aging-Curve Projection"),
      div(class = "gt_table_wrap", gt_output("active_aging"))
    )
  )
)
