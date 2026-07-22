# =============================================================================
# Current-season supplement — data past the snapshot's DATA_SEASON that Lahman
# hasn't published yet (award winners/shares, WAR). This is the declarative
# replacement for the legacy add_row('judgeaa01', 2024, ...) patches and the
# repeated read.csv("AwardShares_2024.csv") blocks.
#
# Schema (long/tidy):  season, playerID, metric, value
#   metric ∈ { MVP_win, MVP_share, CY_win, CY_share, ROY_win, GG_win, SS_win,
#              TC_win, PTC_win, bref_WAR, ... }
#
# When a newer snapshot already contains those seasons, the supplement is empty
# and nothing downstream changes. For the 2024 snapshot the supplement is empty
# because 2024 awards are already in the snapshot natively.
# =============================================================================

suppressMessages({ library(readr); library(dplyr); library(tibble) })

.empty_supplement <- function() {
  tibble::tibble(season = integer(), playerID = character(),
                 metric = character(), value = double())
}

#' Load the configured supplement (or an empty frame if none).
load_supplement <- function(cfg) {
  if (is.null(cfg$supplement)) return(.empty_supplement())
  path <- file.path(cfg$paths$data, "supplement", cfg$supplement)
  if (!file.exists(path)) stop(sprintf("supplement not found: %s", path), call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

#' Winner slice: rows of a *_win metric with value == 1, as (playerID, yearID, <col>="Y").
#' bind_rows()-ing this is a no-op on an empty supplement.
sup_win <- function(sup, metric, col) {
  s <- dplyr::filter(sup, .data$metric == !!metric, .data$value == 1)
  out <- dplyr::tibble(playerID = s$playerID, yearID = as.integer(s$season))
  out[[col]] <- rep("Y", nrow(out))
  out
}

#' Share slice: rows of a *_share metric, as (playerID, yearID, <col>=numeric value).
sup_share <- function(sup, metric, col) {
  s <- dplyr::filter(sup, .data$metric == !!metric)
  out <- dplyr::tibble(playerID = s$playerID, yearID = as.integer(s$season))
  out[[col]] <- as.numeric(s$value)
  out
}
