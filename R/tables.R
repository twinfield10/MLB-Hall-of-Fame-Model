# =============================================================================
# Table provider — the single seam through which feature code reads data.
#
# Legacy code read Batting/Pitching from CSV but every other table from the
# installed `Lahman::` package (an OLDER vintage), which forced the manual
# add_row() award patches. Here every table comes from ONE snapshot via
# get_table(); init_tables(cfg) sets the active config, tbl(name) serves
# (and caches) each table. This removes the vintage split entirely.
# =============================================================================

if (!exists("get_table"))       source(file.path("R", "data_snapshot.R"))
if (!exists("load_supplement")) source(file.path("R", "data_supplement.R"))

.tbl_env <- new.env(parent = emptyenv())

#' Point the provider at a config; call once before assembling features.
#' Also loads the current-season supplement (empty if none configured).
init_tables <- function(cfg) {
  .tbl_env$cfg        <- cfg
  .tbl_env$cache      <- list()
  .tbl_env$supplement <- load_supplement(cfg)
  invisible(cfg)
}

#' The current-season supplement bound by init_tables().
sup <- function() {
  if (is.null(.tbl_env$supplement)) load_supplement(.tbl_env$cfg) else .tbl_env$supplement
}

#' Serve a snapshot table (schema-normalised, cached).
tbl <- function(name) {
  if (is.null(.tbl_env$cfg)) stop("init_tables(cfg) must be called first", call. = FALSE)
  if (is.null(.tbl_env$cache[[name]])) {
    .tbl_env$cache[[name]] <- get_table(.tbl_env$cfg, name)
  }
  .tbl_env$cache[[name]]
}

#' The config the provider is currently bound to.
active_cfg <- function() .tbl_env$cfg
