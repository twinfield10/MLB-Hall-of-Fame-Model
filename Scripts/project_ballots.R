#!/usr/bin/env Rscript
# =============================================================================
# Future-cycle BBWAA ballot projection — carry-forward driver.
#
#   Rscript Scripts/project_ballots.R 2027          # single cycle
#   Rscript Scripts/project_ballots.R 2027 2030     # chain 2027..2030
#   Rscript Scripts/project_ballots.R 2027 2030 --refresh   # re-derive ballots
#
# For each cycle N (in order) it derives the ballot (first-time eligibles +
# surviving holdovers), scores it through the UNCHANGED committee engine, and
# writes outputs/predictions/hof_bbwaa_*_N.csv + Data/ballots/ballot_N.yml + the
# chart. Each cycle's predictions feed the next cycle's holdovers, so the loop
# MUST run in ascending order.
#
# Holdovers are grounded in REAL results where available (snapshot + any
# Data/ballots/results_<year>.csv), and in the model's own predictions for cycles
# beyond the data horizon. See R/ballot_project.R.
# =============================================================================

setwd(Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model"))

.R_FILES <- c("data_snapshot", "config", "data_supplement", "tables", "data_live",
              "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
              "model", "evaluate", "committee_bbwaa", "pipeline", "ballot_project")
suppressMessages(for (f in .R_FILES) source(file.path("R", paste0(f, ".R"))))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

args    <- commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% args
nums    <- suppressWarnings(as.integer(args[!grepl("^--", args)]))
nums    <- nums[!is.na(nums)]
if (!length(nums)) stop("usage: project_ballots.R <start> [end] [--refresh]", call. = FALSE)
start <- nums[1]
end   <- if (length(nums) >= 2) nums[2] else start
if (end < start) stop("end must be >= start", call. = FALSE)

# --- snapshot horizon: derive max_data_lag override + hard ceiling ----------
cfg0 <- yaml::read_yaml("config.yml")
data_dir  <- cfg0$paths$data %||% "Data"
manifest  <- yaml::read_yaml(file.path(data_dir, "snapshots", cfg0$snapshot, "MANIFEST.yml"))
data_seas <- as.integer(manifest$data_season)
elig_lag  <- cfg0$windows$eligible_lag %||% 6
horizon   <- data_seas + elig_lag        # first-timers need finalGame <= data_season
if (end > horizon)
  stop(sprintf(paste("cannot project %d: snapshot '%s' (data_season %d) supports cycles",
                     "through %d only (first-timers must have retired by the data season).",
                     "Refresh the snapshot to go further."),
               end, cfg0$snapshot, data_seas, horizon), call. = FALSE)
lag_override <- max(cfg0$max_data_lag %||% 2, end - data_seas)

message(sprintf("Projecting BBWAA ballots %d..%d | snapshot %s (data_season %d) | horizon %d",
                start, end, cfg0$snapshot, data_seas, horizon))

war <- NULL
for (N in start:end) {
  message(sprintf("\n########## %d ##########", N))
  cfg <- load_config("config.yml", season = N,
                     overrides = list(max_data_lag = lag_override))
  if (is.null(war)) war <- load_war(cfg)   # cached; load once, reuse across cycles

  project_one_season(cfg, war, refresh = refresh)

  # Best-effort chart render (needs gt + mlbplotR); never fatal to the projection.
  chart <- try(system2("Rscript", c("Scripts/pretty_bbwaa_ballot.R", N),
                       stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (inherits(chart, "try-error") || !is.null(attr(chart, "status"))) {
    message(sprintf("   (chart skipped for %d — run Scripts/pretty_bbwaa_ballot.R %d manually)", N, N))
  } else {
    message(sprintf("   %s", tail(chart, 1)))
  }
}
message("\nDone.")
