# =============================================================================
# Shiny app — server. Renders the gt tables reactively from the dropdowns and
# active-player filters. All table styling lives in R/report_gt.R.
# =============================================================================

server <- function(input, output, session) {

  output$future_tbl <- render_gt({
    req(input$future_year)
    y <- as.integer(input$future_year)
    build_ballot_gt(FUTURE[[as.character(y)]], season = y, historical = FALSE)
  })

  output$hist_tbl <- render_gt({
    req(input$hist_year)
    y <- as.integer(input$hist_year)
    build_ballot_gt(HIST[[as.character(y)]], season = y, historical = TRUE)
  })

  # Shared active-player filter: Team (multi), Position (multi), Age range.
  filter_active <- function(d) {
    if (is.null(d) || !nrow(d)) return(d)
    if (length(input$team_f)) d <- d[!is.na(d$team) & d$team %in% input$team_f, , drop = FALSE]
    if (length(input$pos_f))  d <- d[!is.na(d$Pos)  & d$Pos  %in% input$pos_f,  , drop = FALSE]
    if (!is.null(input$age_f))
      d <- d[!is.na(d$Age) & d$Age >= input$age_f[1] & d$Age <= input$age_f[2], , drop = FALSE]
    d
  }

  output$active_frozen <- render_gt(build_active_gt(filter_active(ACTIVE_FROZEN), kind = "frozen"))
  output$active_aging  <- render_gt(build_active_gt(filter_active(ACTIVE_AGING),  kind = "aging"))
}
