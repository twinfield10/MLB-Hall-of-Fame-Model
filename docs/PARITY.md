# Parity report — Phase 2 (single-snapshot data source)

Phase 2 rewired every data read to a single Lahman **snapshot** (`Data/snapshots/lahman_2024/`)
via `tbl()`, removing the old split where Batting/Pitching came from CSV but everything else
came from the older installed `Lahman` package (v12.0.0, data through 2023). The manual
`add_row('judgeaa01', 2024, ...)` award patches and the repeated `AwardShares_2024.csv` reads
are gone — 2024 awards are now native in the snapshot. Baseball-Reference WAR is cached for
determinism. **Dates were left hardcoded in this phase** (Phase 3 parameterizes them), so any
change is attributable to the data source alone.

## Result (measured on players in BOTH the new and golden test sets)

| model    | common | HOF_Prob corr | predict agreement | flips |
|----------|-------:|--------------:|------------------:|------:|
| batters  |    471 |         0.917 |             98.9% |     5 |
| pitchers |   1119 |         0.904 |             99.8% |     2 |

The refactored pipeline's `predict=Y` lists are sensible: batters — Beltré, Cabrera, Bonds,
Cano, Pujols, A-Rod, McGwire, Ramírez, Sosa, Wright, Tejada; pitchers — Clemens, Schilling,
Greinke, Pettitte, K-Rod, Nathan, Papelbon.

## Why membership differs (expected, not a bug)

Golden was produced with a different data vintage, so the two test sets don't contain the same
players. The differences are fully explained:

1. **Active players lose their `finalGame` (dominant effect).** The 2024 snapshot's `People.csv`
   has `finalGame = NA` for ~1,441 currently-active players (Trout, Judge, Altuve, Alonso,
   Acuña, Abreu, …), so they fail the `finalGame ∈ [2012, 2023]` test-window filter and drop
   out. The old Lahman package (through 2023) gave them a 2023 `finalGame`, so golden included
   them. This accounts for the bulk of the 333 (batters) / 590 (pitchers) "golden-only" players.
   → **Product impact:** the pipeline currently does not score active players. Fixed in Phase 3
   by deriving an effective last-season from `max(yearID)` and coalescing with `People.finalGame`.
2. **2025 HOF labels.** Snapshot `HallOfFame` runs through 2025 (vs the package's 2024), so the
   2025 induction class is labeled `inducted = Y` and a few players move between cohorts.
3. **Fresh Baseball-Reference WAR** (pulled today vs golden's Nov-2025 pull) shifts WAR/JAWS
   slightly for recent seasons.
4. **Historical revisions** between Lahman 12.0.0 and the 2024 release (minor stat corrections).

## Reproduce

```bash
Rscript run.R                 # writes outputs/predictions/ + outputs/models/
Rscript scripts/parity_report.R
```
