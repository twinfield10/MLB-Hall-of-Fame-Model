# =============================================================================
# Live external data — Baseball-Reference WAR (war_daily_bat / war_daily_pitch)
#
# The legacy code pulled these over HTTP on every run, so runs were not
# reproducible. Here the pull is cached (fetch-date stamped) under Data/cache/;
# the pipeline reads the cache by default and only re-pulls when live_refresh
# is TRUE. This makes a run deterministic and offline-capable.
# =============================================================================

suppressMessages(library(readr))

# Column type specs are the historic ones from the legacy pull_br_data().
.BR_BAT_DTYPE   <- "ciiciciccicdddddddddddddddddddddddccddddddddddddd"
.BR_PITCH_DTYPE <- "ciiciciciiiddidddiddiddddddddddddddddddddd"

#' Fetch BR WAR daily files. Returns list(BattingWAR, PitchingWAR, fetched_at).
pull_br_war <- function(timeout = 120) {
  old <- options(timeout = timeout); on.exit(options(old), add = TRUE)
  bat <- readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt",
                         col_types = .BR_BAT_DTYPE, progress = FALSE)
  pit <- readr::read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt",
                         col_types = .BR_PITCH_DTYPE, progress = FALSE)
  list(BattingWAR = bat, PitchingWAR = pit, fetched_at = as.character(Sys.Date()))
}

#' Return BR WAR for the pipeline: cached by default, re-pulled if live_refresh.
#' Writes both a canonical cache (br_war.rds) and a dated provenance copy.
load_war <- function(cfg) {
  cache_dir <- file.path(cfg$paths$data, "cache")
  canonical <- file.path(cache_dir, "br_war.rds")
  if (!isTRUE(cfg$live_refresh) && file.exists(canonical)) {
    return(readRDS(canonical))
  }
  war <- pull_br_war()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  saveRDS(war, canonical)
  saveRDS(war, file.path(cache_dir, sprintf("br_war_%s.rds", war$fetched_at)))
  war
}
