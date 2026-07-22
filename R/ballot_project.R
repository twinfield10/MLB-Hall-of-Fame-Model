# =============================================================================
# Future-cycle ballot projection.
#
# The historical pipeline scores a MANUAL ballot (Data/ballots/ballot_<season>.yml).
# For a cycle that has not been named yet we must DERIVE the candidate pool from
# BBWAA eligibility rules and CHAIN cycle-to-cycle:
#
#   cohort(N) = holdovers(N)  U  first_timers(N)
#     holdovers(N)    : prior-cycle share in [survive_pct, elect_pct), within the
#                       10-year ballot tenure  (from ballot_state_asof)
#     first_timers(N) : finalGame year == N-6, >=10 seasons played, passing a
#                       calibrated WAR|JAWS|Monitor screen, not banned/inducted
#
# Chaining: the prior cycle's OUTCOME sets every holdover's prev_share /
# appearance_number / crowding. Real cycles come from the snapshot + known-actual
# results (build_ballot_panel, folded into training too). Cycles the data does not
# yet cover are supplied by the model's OWN prior-cycle predictions
# (outputs/predictions/hof_bbwaa_*_<cycle>.csv), injected into the SCORING-state
# panel only (never training) via cohorts$projected in .run_bbwaa.
#
# Driver: Scripts/project_ballots.R <start> <end>.
# =============================================================================

suppressMessages({ library(dplyr); library(readr) })

if (!exists("build_ballot_panel")) source(file.path("R", "ballot_features.R"))
if (!exists("build_cohorts"))      source(file.path("R", "cohorts.R"))
if (!exists("run_pipeline"))       source(file.path("R", "pipeline.R"))

`%||%`   <- function(a, b) if (is.null(a) || length(a) == 0) b else a
`%notin%` <- Negate(`%in%`)

# ---- projected-cycle collection (the carry-forward medium) ------------------

#' Last ballot year present in `panel` — the boundary between REAL cycles
#' (snapshot + known-actual results, already in the panel & training) and the
#' PREDICTED future cycles that must be synthesised for scoring.
last_real_ballot_year <- function(panel) suppressWarnings(max(panel$yearID, na.rm = TRUE))

#' Synthetic ballot rows for one predicted `cycle`, from its persisted prediction
#' CSVs. Only (playerID, yearID, share) drive ballot_state_asof; `elected` marks
#' who leaves the ballot. Returns NULL if the cycle's predictions are absent.
read_projected_cycle <- function(cfg, cycle) {
  elect <- cfg$bbwaa$elect_pct %||% 0.75
  rule  <- cfg$bbwaa$project$elected_rule %||% "share"
  files <- file.path(cfg$paths$outputs, "predictions",
                     sprintf("hof_bbwaa_%ss_%d.csv", c("batter", "pitcher"), cycle))
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)
  d <- dplyr::bind_rows(lapply(files, function(f)
    readr::read_csv(f, show_col_types = FALSE, progress = FALSE)))
  if (!nrow(d)) return(NULL)
  elected <- if (identical(rule, "prob") && "P_elected_cycle" %in% names(d)) {
    d$P_elected_cycle > 0.5
  } else {
    d$pred_share >= elect
  }
  dplyr::tibble(playerID = d$playerID, yearID = as.integer(cycle),
                share = as.numeric(d$pred_share), elected = elected)
}

#' Bind all predicted cycles in (last_real+1)..(season-1). Empty (NULL) when the
#' real/known data already reaches season-1 — the graceful no-op that makes a
#' snapshot refresh transparently replace the chain.
collect_projected_cycles <- function(cfg, panel, season) {
  lo <- last_real_ballot_year(panel) + 1L
  hi <- as.integer(season) - 1L
  if (lo > hi) return(NULL)
  rows <- Filter(Negate(is.null), lapply(lo:hi, function(y) read_projected_cycle(cfg, y)))
  if (!length(rows)) return(NULL)
  missing <- setdiff(lo:hi, unique(unlist(lapply(rows, function(r) r$yearID))))
  if (length(missing))
    stop(sprintf("projecting %d needs predicted cycle(s) %s — run those seasons first",
                 season, paste(missing, collapse = ", ")), call. = FALSE)
  dplyr::bind_rows(rows)
}

# ---- cohort derivation ------------------------------------------------------

#' Distinct seasons played per player (Batting U Pitching) — proxy for the
#' "10 championship seasons" rule. Returns a named integer vector.
.seasons_played <- function(ids) {
  yrs <- dplyr::bind_rows(
    tbl("Batting")  %>% dplyr::filter(playerID %in% ids) %>% dplyr::distinct(playerID, yearID),
    tbl("Pitching") %>% dplyr::filter(playerID %in% ids) %>% dplyr::distinct(playerID, yearID)
  ) %>% dplyr::distinct(playerID, yearID)
  tapply(yrs$yearID, yrs$playerID, function(x) length(unique(x)))
}

#' Career screen columns (CareerWAR, JAWS, HOFM) from a merit matrix.
.merit_screen_cols <- function(merit) {
  war_cols <- grep("^WAR_[0-9]+$", names(merit), value = TRUE)
  dplyr::tibble(
    playerID  = merit$playerID,
    CareerWAR = if (length(war_cols)) rowSums(merit[, war_cols, drop = FALSE], na.rm = TRUE) else 0,
    JAWS      = if ("jaws"        %in% names(merit)) as.numeric(merit$jaws)        else 0,
    HOFM      = if ("HOFM_Points" %in% names(merit)) as.numeric(merit$HOFM_Points) else 0
  )
}

#' First-time eligibles for `season`: finalGame year == season-6, >=10 seasons
#' played, passing the calibrated OR-screen (CareerWAR | JAWS | Monitor). Excludes
#' banned, already-inducted, and anyone with a prior BBWAA ballot appearance.
derive_first_timers <- function(season, cfg, merit_bat, merit_pit, panel = NULL) {
  ft_year <- cfg$dates$eligible_max_yr          # == season - eligible_lag (== season-6)

  retired <- tbl("People") %>%
    dplyr::transmute(playerID,
                     final_year = suppressWarnings(as.integer(substr(as.character(finalGame), 1, 4)))) %>%
    dplyr::filter(!is.na(final_year), final_year == ft_year) %>%
    dplyr::pull(playerID)
  if (!length(retired)) return(character(0))

  sp   <- .seasons_played(retired)
  ge10 <- names(sp)[sp >= 10L]
  if (!length(ge10)) return(character(0))

  scr   <- cfg$bbwaa$project %||% list()
  merit <- dplyr::bind_rows(.merit_screen_cols(merit_bat), .merit_screen_cols(merit_pit))
  kept  <- merit %>%
    dplyr::filter(playerID %in% ge10,
                  (CareerWAR >= (scr$screen_war  %||% 22)) |
                  (JAWS      >= (scr$screen_jaws %||% 22)) |
                  (HOFM      >= (scr$screen_hofm %||% 70))) %>%
    dplyr::pull(playerID)

  banned   <- unlist(cfg$cohorts$banned_ids)
  inducted <- tbl("HallOfFame") %>%
    dplyr::filter(inducted == "Y", category == "Player") %>% dplyr::pull(playerID)
  prior    <- if (is.null(panel)) character(0) else unique(panel$playerID)

  unique(setdiff(kept, c(banned, inducted, prior)))
}

#' Holdovers entering `season`: a candidate who was on the IMMEDIATELY PRIOR
#' cycle's ballot (year season-1), was not elected there, cleared the 5% survival
#' line, and has ballot tenure left. Keying off the season-1 row (not merely a
#' player's most-recent-ever share) is essential — otherwise a long-retired player
#' whose final BBWAA appearance decades ago happened to land in [5%, 75%) would be
#' mistaken for a current holdover.
derive_holdovers <- function(season, panel_state, cfg) {
  surv   <- cfg$bbwaa$survive_pct %||% 0.05
  elect  <- cfg$bbwaa$elect_pct   %||% 0.75
  banned <- unlist(cfg$cohorts$banned_ids)
  py     <- as.integer(season) - 1L
  appcts <- panel_state %>% dplyr::filter(yearID <= py) %>%
    dplyr::count(playerID, name = "n_appear")
  panel_state %>%
    dplyr::filter(yearID == py) %>%                      # on the prior cycle's ballot
    dplyr::left_join(appcts, by = "playerID") %>%
    dplyr::filter(!elected,                              # not elected that cycle
                  share >= surv, share < elect,          # survived, below election
                  n_appear < bbwaa_ballot_max_years(season),   # tenure left for `season`
                  playerID %notin% banned) %>%
    dplyr::pull(playerID)
}

#' Full derived cohort for `season`: holdovers U first-timers, with manual
#' add_ids/remove_ids overrides. Returns a list(all, holdovers, first_timers).
derive_ballot_cohort <- function(season, panel_state, cfg, merit_bat, merit_pit,
                                 add_ids = character(0), remove_ids = character(0)) {
  holdovers    <- derive_holdovers(season, panel_state, cfg)
  first_timers <- setdiff(derive_first_timers(season, cfg, merit_bat, merit_pit,
                                              panel = panel_state), holdovers)
  all_ids <- unique(c(setdiff(c(holdovers, first_timers), remove_ids), add_ids))
  list(all = all_ids,
       holdovers    = intersect(all_ids, holdovers),
       first_timers = intersect(all_ids, first_timers))
}

# ---- reviewable ballot YAML -------------------------------------------------

#' Write a hand-editable Data/ballots/ballot_<season>.yml with full playerIDs
#' (holdovers + first_timers, annotated with name & prior share). build_cohorts()
#' consumes it via the `full_ids` path; add_ids/remove_ids let an analyst override.
write_ballot_yaml <- function(path, season, coh, panel_state, cfg) {
  nm <- tbl("People") %>%
    dplyr::transmute(playerID, nm = trimws(paste(nameFirst, nameLast))) %>%
    { setNames(.$nm, .$playerID) }
  st   <- ballot_state_asof(panel_state, season)
  prev <- setNames(st$prev_share, st$playerID)
  item <- function(id) {
    p   <- if (id %in% names(prev)) prev[[id]] else NA_real_
    tag <- if (!is.na(p)) sprintf(" (prev %.3f)", p) else ""
    sprintf("    - %-12s # %s%s", id, nm[[id]] %||% "?", tag)
  }
  none <- function(v) if (!length(v)) "    []" else NULL
  txt <- c(
    sprintf("# Derived %d BBWAA ballot — generated by Scripts/project_ballots.R.", season),
    "# `full_ids` win over stub resolution (see R/cohorts.R). Hand-edit add_ids /",
    "# remove_ids to override the screen or fix an edge case, then re-run.",
    sprintf("season: %d", season),
    "source: derived",
    "full_ids:",
    "  holdovers:",
    if (length(coh$holdovers)) vapply(coh$holdovers, item, character(1)) else "    []",
    "  first_timers:",
    if (length(coh$first_timers)) vapply(coh$first_timers, item, character(1)) else "    []",
    "add_ids: []",
    "remove_ids: []"
  )
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  writeLines(txt, path)
  invisible(path)
}

#' Resolve an existing (possibly hand-edited) derived ballot YAML to playerIDs,
#' reusing R/cohorts.R's full_ids logic.
.read_derived_ballot_ids <- function(yml_path, cfg) {
  cfg2 <- cfg; cfg2$ballot_file <- basename(yml_path)
  cfg2$paths$data <- dirname(dirname(yml_path))
  bf <- read_ballot_file(cfg2)
  if (!is.null(bf$full_ids)) unique(c(setdiff(bf$full_ids, bf$rem_ids), bf$add_ids))
  else find_hof_ballot_ids(l = bf$short_ids, last_year = bf$last_year,
                           rem_ids = bf$rem_ids, add_ids = bf$add_ids)
}

# ---- per-season orchestration ----------------------------------------------

#' Project ONE cycle end-to-end: build panel (+known-actuals) & merit, collect
#' predicted prior cycles, derive-or-read the ballot, then score batter+pitcher
#' through the unchanged committee engine (cohorts$projected carries the chain).
#' Writes hof_bbwaa_*_<season>.csv (+ model/metrics) and the reviewable YAML.
#'
#' @param refresh re-derive the ballot even if ballot_<season>.yml already exists.
project_one_season <- function(cfg, war, refresh = FALSE) {
  season <- cfg$season
  init_tables(cfg)

  panel     <- build_ballot_panel(label_max_year = cfg$dates$label_max_yr)
  merit_bat <- build_merit_matrix(war, "batter",  cfg)
  merit_pit <- build_merit_matrix(war, "pitcher", cfg)

  projected   <- collect_projected_cycles(cfg, panel, season)
  panel_state <- if (is.null(projected)) panel else dplyr::bind_rows(panel, projected)

  yml <- file.path(cfg$paths$data, "ballots", sprintf("ballot_%d.yml", season))
  if (file.exists(yml) && !refresh) {
    ballot_ids <- .read_derived_ballot_ids(yml, cfg)
    coh <- list(all = ballot_ids,
                holdovers    = intersect(ballot_ids, derive_holdovers(season, panel_state, cfg)),
                first_timers = NULL)
    message(sprintf("   ballot: read %d candidates from %s", length(ballot_ids), yml))
  } else {
    coh <- derive_ballot_cohort(season, panel_state, cfg, merit_bat, merit_pit)
    write_ballot_yaml(yml, season, coh, panel_state, cfg)
    ballot_ids <- coh$all
    message(sprintf("   ballot: derived %d candidates (%d holdovers + %d first-timers) -> %s",
                    length(ballot_ids), length(coh$holdovers), length(coh$first_timers), yml))
  }

  cohorts <- list(
    vetcom    = pull_hof_vet_eligible(vetcom_years     = cfg$dates$vetcom_years,
                                      ballot_max_years = cfg$thresholds$ballot_max_years),
    ballot    = ballot_ids,
    projected = projected
  )

  results <- lapply(c("batter", "pitcher"), function(pt) {
    message(sprintf("== %s ==", pt))
    res <- run_pipeline(cfg, pt, war = war, cohorts = cohorts)
    message(sprintf("   wrote %s (%d candidates; share MAE %.3f | elect AUC %.3f | n=%d)",
                    res$out, nrow(res$results), res$metrics$share_mae,
                    res$metrics$elect_roc_auc, res$metrics$n))
    res
  })
  names(results) <- c("batter", "pitcher")

  list(season = season, cohort = coh, projected = projected,
       ballot_yaml = yml, results = results)
}
