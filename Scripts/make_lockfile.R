#!/usr/bin/env Rscript
# Generate a restorable renv.lock from the CURRENTLY INSTALLED package versions,
# without activating renv (which would repoint the library in a shared setup).
# Captures the recursive dependency closure of DESCRIPTION's Imports.
#   Rscript scripts/make_lockfile.R
suppressMessages(library(jsonlite))

desc  <- read.dcf("DESCRIPTION")
imports <- trimws(strsplit(gsub("\n", " ", desc[, "Imports"]), ",")[[1]])
imports <- gsub("\\s*\\(.*\\)", "", imports)          # strip version constraints

ip   <- installed.packages()
inst <- as.data.frame(ip, stringsAsFactors = FALSE)
closure <- tools::package_dependencies(imports, db = ip, recursive = TRUE,
                                       which = c("Depends", "Imports", "LinkingTo"))
pkgs <- sort(unique(c(imports, unlist(closure))))
pkgs <- pkgs[pkgs %in% inst$Package]
pkgs <- setdiff(pkgs, c("R"))

records <- setNames(lapply(pkgs, function(p) {
  v <- inst$Version[inst$Package == p][1]
  base_prio <- inst$Priority[inst$Package == p][1]
  if (!is.na(base_prio) && base_prio == "base") {
    list(Package = p, Version = v, Source = "R")
  } else {
    list(Package = p, Version = v, Source = "Repository", Repository = "CRAN")
  }
}), pkgs)

lock <- list(
  R = list(
    Version = as.character(getRversion()),
    Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))
  ),
  Packages = records
)
writeLines(toJSON(lock, auto_unbox = TRUE, pretty = TRUE, null = "null"), "renv.lock")
cat(sprintf("renv.lock written: %d packages (R %s)\n", length(pkgs), getRversion()))
