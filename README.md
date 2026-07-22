# MLB Hall of Fame Model

Predicts each player's probability of election to the Baseball Hall of Fame from
historical [Lahman](https://sabr.org/lahman-database/) data plus Baseball-Reference
WAR. Separate models for batters and pitchers.

The Hall is elected by distinct bodies, so the model is **committee-specific**
(`model.committee` in `config.yml`):

- **`bbwaa`** *(default)* — the Baseball Writers' vote, the main and most predictable
  route. Uses ballot history (prior-year vote share, appearances, momentum) to project
  each candidate's ballot share, `P(elected this cycle)`, and `P(ever elected)`.
  See `docs/MODELING.md`.
- **`any_route`** — the original model: `P(inducted by any body)` as a single `HOF_Prob`.

Veterans/Era-committee models are a later addition.

This repository is a **reproducible refactor** of a legacy interactive R workflow.
The design goal: **rolling to a new season = change one config value + drop in a new
data snapshot + run one command.** No code edits, no scattered hardcoded years.

> **Status (in progress).** The refactor is phased and *parity-gated*: it reproduces
> the current 2026 predictions exactly before any behavior changes. See "Roadmap".

## Layout

```
config.yml            # the single knob (`season`) + all raw inputs
run.R                 # one-command orchestrator: load -> features -> train -> predict
R/                     # function library (sourced in dependency order by run.R)
  config.R            #   load_config(): derives every date from `season` + DATA_SEASON
  data_snapshot.R     #   get_table(), build_manifest(), validate_vintage()
  data_supplement.R   #   current-season overrides (replaces the add_row hacks)
  data_live.R         #   Baseball-Reference WAR pull, cached
  tables.R            #   table provider bound to the active snapshot
  features.R          #   append_*() feature builders
  feature_matrix.R    #   build_fit_matrix()/aggregate_career()/append_war() (shared core)
  assemble.R          #   the batter/pitcher feature-assembly chains
  cohorts.R           #   VetCom-eligible + BBWAA ballot cohorts (reads Data/ballots/)
  ballot_features.R   #   BBWAA player-ballot-year panel + ballot-history features
  model.R             #   engine registry, train/predict, calibration, persistence
  evaluate.R          #   held-out metrics from CV out-of-fold predictions
  committee_bbwaa.R   #   BBWAA committee model: share regressor + forward simulation
  labels.R            #   experimental 4-class label + multinomial model
  report.R            #   gt "Cooperstown likelihood" tables
Data/
  snapshots/lahman_2025/   # immutable dated Lahman CSVs + MANIFEST.yml (source of truth)
  supplement/              # current-season data not yet in a snapshot (awards/WAR)
  ballots/                 # per-cycle BBWAA ballot (irreducibly manual input)
  cache/                   # cached live pulls (git-ignored)
outputs/{models,predictions,metrics,reports}/
scripts/              # helper CLIs (parity, report, experiments, lockfile)
tests/
  testthat/           # 41 unit tests (temporal derivation, features, metrics, 4-class)
  golden/             # frozen 2024-data predictions — the regression oracle
archive/legacy/       # the original scripts, kept for reference, never sourced
Scripts/PrettyHOFCharts.R   # the original chart code, kept as report reference
```

> Orchestration is a straight `run.R` today. A `{targets}` DAG (per-stage caching /
> dependency tracking) is a natural future upgrade but wasn't needed to hit the goal.

## The temporal design (why this refactor exists)

Two anchors drive every date; nothing temporal is hardcoded elsewhere:

- **`season`** — the induction cycle being predicted (the yearly knob in `config.yml`).
- **`DATA_SEASON`** — the max `yearID` in the snapshot, **read from `MANIFEST.yml`** and
  validated, never typed.

`R/config.R::load_config()` derives the train/test windows, HOF-eligibility cutoff, the
"active player" threshold, player age reference, and the Veterans-Committee scan window
from those two. `validate_vintage()` fails loudly if the snapshot is stale
(`season - DATA_SEASON > max_data_lag`) with no supplement, or if `Batting`/`Pitching`
disagree on the data season.

## Rolling to a new season (the intended workflow)

1. Download the latest Lahman CSV release; place it under `Data/snapshots/lahman_<YEAR>/`.
2. `Rscript -e 'source("R/data_snapshot.R"); build_manifest("Data/snapshots/lahman_<YEAR>")'`
3. In `config.yml`: set `season:` and point `snapshot:` at the new folder.
4. If the ballot/current-season data isn't in the snapshot yet, add a
   `Data/supplement/supplement_<YEAR>.csv` and `Data/ballots/ballot_<SEASON>.yml`.
5. `Rscript run.R` — rebuilds features, models, predictions, metrics, and report.

## Reproducibility

- Dependencies are declared in `DESCRIPTION` and pinned in `renv.lock` (143 packages,
  R 4.3.2). Rebuild the exact environment with `renv::restore()`. Regenerate the lock
  after changing deps with `Rscript scripts/make_lockfile.R`.
- The Baseball-Reference WAR pull is cached (fetch-date stamped) under `Data/cache/` so a
  run is deterministic and offline-capable; set `live_refresh: true` to re-pull.
- Trained models persist to `outputs/models/<type>_<season>.rds` with a `.meta.yml`
  sidecar (season, data vintage, snapshot, seed, engine, feature columns, metrics);
  metrics to `outputs/metrics/`.

## Commands

```bash
Rscript run.R                      # train + predict (batters & pitchers) -> outputs/
Rscript scripts/render_report.R    # gt likelihood tables -> outputs/reports/*.html
Rscript scripts/parity_report.R    # compare predictions to tests/golden/
Rscript scripts/modeling_experiments.R   # compare imbalance / calibration / 4-class
Rscript -e 'testthat::test_dir("tests/testthat")'   # unit tests (41)
```

See [docs/PARITY.md](docs/PARITY.md) and [docs/MODELING.md](docs/MODELING.md) for detail.

## Roadmap

- [x] **Phase 0** — freeze golden outputs; archive legacy scripts; git init.
- [x] **Phase 1** — scaffolding; `config.yml` + derivation; snapshot manifest + validation.
- [x] **Phase 2** — single-snapshot data layer (removed the `Lahman::`-vs-CSV split & award
      hacks; cached BR WAR). Parity vs golden: batters 0.917 corr / 98.9% agreement,
      pitchers 0.904 / 99.8%. See [docs/PARITY.md](docs/PARITY.md).
- [x] **Phase 3** — all dates derived from `season`/`data_season` (nothing hardcoded);
      active players' `finalGame` coalesced from last season played (so they're scored
      again); HOF labels capped at `season-1` so we never peek at the season being
      predicted. Validated by rolling the snapshot forward to **2025** (`Rscript run.R`),
      producing a clean 2026 forward prediction.
- [x] **Phase 4** — de-duplicated the batter/pitcher paths into one core
      (`R/feature_matrix.R`: `build_fit_matrix`/`aggregate_career`/`append_war`; the
      `get_fit_data`/`p_*` functions are now thin wrappers), removed dead code
      (`convert_to_factors`), fixed the `MPVWin` typo. **Byte-identical** to the Phase-3
      baseline (max |HOF_Prob diff| = 0, 0 prediction changes). Ported scripts archived.
- [x] **Phase 5** — model module (engine registry in `R/model.R`), persistence with a
      `.meta.yml` sidecar, and held-out evaluation from cross-validated out-of-fold
      predictions (`R/evaluate.R`): AUC / PR-AUC / log-loss / Brier / calibration /
      precision@k. Parity preserved. See [docs/MODELING.md](docs/MODELING.md).
- [x] **Phase 6** — modeling options, all config-gated: class-imbalance (`up`/`down`/
      `weights`), probability calibration (`platt`/`isotonic`), and an experimental 4-class
      label (`FirstBallot`/`BBWAA`/`VetCom`/`No`, `R/labels.R`). Evaluation showed the
      baseline already near-optimal, so defaults stay `none`/`none`/`binary`.
- [x] **Phase 7** — `gt` report (`R/report.R` → `outputs/reports/`), a restorable
      `renv.lock`, and docs.
