# Locate the project root (dir containing config.yml) and load the function library.
# Runs once before the test files; sets wd to root so relative paths in R/ resolve.
find_root <- function(start = getwd()) {
  d <- normalizePath(start)
  while (!file.exists(file.path(d, "config.yml")) && dirname(d) != d) d <- dirname(d)
  d
}
PROJ_ROOT <- find_root()
setwd(PROJ_ROOT)
for (f in c("data_snapshot", "config", "data_supplement", "tables", "data_live",
            "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
            "model", "evaluate", "labels", "committee_bbwaa", "pipeline", "ballot_project",
            "active_players", "career_projection"))
  suppressWarnings(suppressMessages(source(file.path(PROJ_ROOT, "R", paste0(f, ".R")))))
