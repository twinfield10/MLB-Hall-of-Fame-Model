#!/usr/bin/env Rscript
# =============================================================================
# Active-player HOF forecast — Monte-Carlo career projection.
#
#   Rscript Scripts/project_active_hof.R            # K = 300 sims
#   Rscript Scripts/project_active_hof.R 500        # K = 500 sims
#
# Projects each active player's REMAINING career (Marcel aging curves) over K
# Monte-Carlo paths, scores each projected final résumé with the any-route merit
# model, and reports P(HOF) with a p10-p90 range + the projected-WAR distribution.
# Writes outputs/predictions/active_hof_projection_<data_season>.csv and a gt
# chart. See R/career_projection.R. Frozen (no-projection) scoring is
# Scripts/active_players.R.
# =============================================================================

setwd(Sys.getenv("HOF_ROOT", unset = "/Users/tommywinfield/GitRepos/MLB HOF Model"))

.R_FILES <- c("data_snapshot", "config", "data_supplement", "tables", "data_live",
              "feature_matrix", "features", "assemble", "cohorts", "ballot_features",
              "model", "evaluate", "committee_bbwaa", "pipeline", "active_players", "career_projection")
suppressMessages(for (f in .R_FILES) source(file.path("R", paste0(f, ".R"))))

args <- commandArgs(trailingOnly = TRUE)
K    <- if (length(args) >= 1) as.integer(args[1]) else 300L

cfg <- load_config("config.yml")
message(sprintf("Active-player HOF projection | data_season %d | K=%d sims", cfg$data_season, K))
init_tables(cfg)
war     <- load_war(cfg)
cohorts <- build_cohorts(cfg)

res <- project_active_hof(cfg, war, cohorts, K = K)

dir <- file.path(cfg$paths$outputs, "predictions")
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
out_csv <- file.path(dir, sprintf("active_hof_projection_%d.csv", cfg$data_season))
utils::write.csv(res, out_csv, row.names = FALSE)
message(sprintf("wrote %s (%d players projected)", out_csv, nrow(res)))

# ---- chart (best-effort) ----------------------------------------------------
render_chart <- function() {
  suppressMessages({ library(gt); library(dplyr) })
  top <- res %>% dplyr::filter(P_HOF >= 0.05) %>% dplyr::arrange(dplyr::desc(P_HOF)) %>%
    dplyr::transmute(
      Name = name, Type = type, Age, Szn = real_seasons, NowWAR = real_WAR,
      Frozen = P_HOF_frozen, Proj = P_HOF, Range = sprintf("%.2f–%.2f", P_HOF_p10, P_HOF_p90),
      ProjWAR = proj_WAR_med, WARRange = sprintf("%.0f–%.0f", proj_WAR_p10, proj_WAR_p90),
      AddYrs = added_years)
  .green <- c("#f7f7f7", "#d6efd6", "#98e6bb", "#46d285", "#1e7b48")
  gt_obj <- top %>% gt() %>%
    cols_label(Name=md("Player"), Type=md("Type"), Age=md("Age"), Szn=md("Szn"), NowWAR=md("WAR now"),
               Frozen=md("Frozen"), Proj=md("Projected"), Range=md("p10–p90"),
               ProjWAR=md("Median"), WARRange=md("p10–p90"), AddYrs=md("+Yrs")) %>%
    tab_spanner(label="Player", columns=c(Name, Type, Age, Szn, NowWAR), id="PLY") %>%
    tab_spanner(label="P(HOF)", columns=c(Frozen, Proj, Range), id="PHOF") %>%
    tab_spanner(label="Projected Career WAR", columns=c(ProjWAR, WARRange, AddYrs), id="PWAR") %>%
    tab_style(style=list(cell_fill(color="#2F4985"), cell_text(weight="bold", style="italic", color="white"),
                         cell_borders(sides=c("right","left","bottom"), color="#BBBBBB", weight=px(2))),
              locations=cells_column_spanners(spanners=c("PLY","PHOF","PWAR"))) %>%
    tab_style(locations=cells_column_labels(), style=cell_text(weight="bold", align="center")) %>%
    tab_style(locations=cells_body(columns=Name), style=cell_text(align="left")) %>%
    data_color(columns=c(Frozen, Proj), palette=.green, domain=c(0,1), na_color="#f7f7f7") %>%
    data_color(columns=c(NowWAR, ProjWAR), palette=.green) %>%
    fmt_percent(columns=c(Frozen, Proj), decimals=1) %>%
    fmt_number(columns=c(NowWAR, ProjWAR), decimals=1) %>%
    cols_align("center", columns=c(Type, Age, Szn, NowWAR, Frozen, Proj, Range, ProjWAR, WARRange, AddYrs)) %>%
    tab_header(title=md("<span style='color:white'>**Active Players — Projected HOF Odds**</span>"),
               subtitle=md(sprintf("<span style='color:white'>*Monte-Carlo career projection (Marcel aging, %d sims). Frozen = career as-is today; Projected = full projected career.*</span>", K))) %>%
    tab_source_note(md(sprintf("_Each sim projects the remaining career (aging curves + WAR/playing-time/longevity noise) and scores the projected résumé with the any-route merit model. P(HOF) = mean over sims; p10–p90 is the career-uncertainty range. Data through %d._", cfg$data_season))) %>%
    tab_options(heading.background.color="#001C5D", data_row.padding=px(4),
                column_labels.border.bottom.color="black", column_labels.border.bottom.width=px(2))
  rdir <- file.path(cfg$paths$outputs, "reports")
  if (!dir.exists(rdir)) dir.create(rdir, recursive=TRUE)
  out_html <- file.path(rdir, sprintf("active_hof_projection_%d.html", cfg$data_season))
  gtsave(gt_obj, out_html)
  message(sprintf("wrote %s (%d players shown)", out_html, nrow(top)))
}
if (inherits(try(render_chart(), silent=TRUE), "try-error"))
  message("   (chart skipped — gt unavailable; CSV written)")
message("Done.")
