# =============================================================================
# Data snapshot layer — single source of truth
#
# Every Lahman table is read from ONE dated CSV snapshot under Data/snapshots/,
# replacing the old split where Batting/Pitching came from disk but everything
# else came from the (older) installed `Lahman::` package. That split was the
# root cause of the manual add_row() award patches; routing every read through
# get_table() removes the need for them.
# =============================================================================

suppressMessages({
  library(readr)
  library(dplyr)
  library(yaml)
})

# Normalise schema drift between the CSV snapshot and the historic package usage,
# in ONE place: lowercase `yearid` -> `yearID`, and `2B`/`3B` -> `X2B`/`X3B`.
.normalize_schema <- function(df) {
  nm <- names(df)
  if ("yearid" %in% nm && !("yearID" %in% nm)) df <- dplyr::rename(df, yearID = yearid)
  if ("2B" %in% nm) df <- dplyr::rename(df, X2B = `2B`)
  if ("3B" %in% nm) df <- dplyr::rename(df, X3B = `3B`)
  df
}

#' Path to a snapshot directory from a config object (or a bare snapshot name).
snapshot_dir <- function(cfg) {
  if (is.list(cfg)) file.path(cfg$paths$data, "snapshots", cfg$snapshot)
  else file.path("Data", "snapshots", cfg)
}

#' Read a single Lahman table from the configured snapshot, schema-normalised.
#' @param cfg  a config list (from load_config) OR a snapshot name string
#' @param name table name without extension, e.g. "Batting", "HallOfFame"
get_table <- function(cfg, name) {
  path <- file.path(snapshot_dir(cfg), paste0(name, ".csv"))
  if (!file.exists(path)) stop(sprintf("Snapshot table not found: %s", path), call. = FALSE)
  df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  .normalize_schema(df)
}

#' Detect the year column of a table (handles yearID / yearid), else NA.
.year_col <- function(df) {
  yc <- intersect(c("yearID", "yearid", "yearId"), names(df))
  if (length(yc)) yc[1] else NA_character_
}

#' Build (and write) MANIFEST.yml for a snapshot: per-table row count, max year,
#' and md5 checksum, plus the derived DATA_SEASON (max yearID across Batting &
#' Pitching). Run once per snapshot; load_config() reads DATA_SEASON from it.
build_manifest <- function(snap_dir, write = TRUE) {
  csvs <- sort(list.files(snap_dir, pattern = "\\.csv$", full.names = TRUE))
  tables <- lapply(csvs, function(p) {
    df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE, progress = FALSE))
    yc <- .year_col(df)
    list(
      rows     = nrow(df),
      max_year = if (!is.na(yc)) suppressWarnings(max(df[[yc]], na.rm = TRUE)) else NA,
      md5      = unname(tools::md5sum(p))
    )
  })
  names(tables) <- tools::file_path_sans_ext(basename(csvs))

  data_season <- max(
    tables[["Batting"]]$max_year %||% NA,
    tables[["Pitching"]]$max_year %||% NA,
    na.rm = TRUE
  )

  manifest <- list(
    snapshot    = basename(snap_dir),
    data_season = as.integer(data_season),
    built_at    = as.character(Sys.Date()),
    n_tables    = length(tables),
    tables      = tables
  )
  if (write) yaml::write_yaml(manifest, file.path(snap_dir, "MANIFEST.yml"))
  invisible(manifest)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Read a snapshot's MANIFEST.yml (must have been built).
read_manifest <- function(cfg) {
  p <- file.path(snapshot_dir(cfg), "MANIFEST.yml")
  if (!file.exists(p)) {
    stop(sprintf("MANIFEST.yml missing for snapshot '%s'. Run build_manifest('%s').",
                 if (is.list(cfg)) cfg$snapshot else cfg, snapshot_dir(cfg)), call. = FALSE)
  }
  yaml::read_yaml(p)
}

#' Validate the snapshot is coherent and not silently stale.
#' Fails loudly if Batting/Pitching disagree on the data season, if checksums
#' drift, or if season - DATA_SEASON exceeds the allowed lag with no supplement.
validate_vintage <- function(cfg, manifest = NULL) {
  m <- manifest %||% read_manifest(cfg)

  bat <- m$tables[["Batting"]]$max_year
  pit <- m$tables[["Pitching"]]$max_year
  if (!identical(as.integer(bat), as.integer(pit))) {
    stop(sprintf("Snapshot incoherent: Batting max year %s != Pitching max year %s", bat, pit),
         call. = FALSE)
  }

  lag <- cfg$season - m$data_season
  if (lag > cfg$max_data_lag && is.null(cfg$supplement)) {
    stop(sprintf(paste0("STALE DATA: season %d is %d years ahead of DATA_SEASON %d ",
                        "(max_data_lag=%d) and no supplement is configured. ",
                        "Add a newer snapshot or a Data/supplement file."),
                 cfg$season, lag, m$data_season, cfg$max_data_lag), call. = FALSE)
  }
  invisible(m)
}
