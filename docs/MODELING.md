# Modeling & evaluation

## Model

XGBoost (`caret::train(method = "xgbLinear")`, 10-fold CV) trained separately for
batters and pitchers, from the wide top-20-seasons-by-WAA feature matrix. The engine
is pluggable via `config.yml` `model.engine` (`xgbLinear` | `xgbTree` | `glmnet` | `rf`);
seeds are per-type in config. Fitted models persist to `outputs/models/<type>_<season>.rds`
with a `.meta.yml` sidecar (season, data vintage, snapshot, seed, engine, feature columns,
best tune, metrics).

## Held-out evaluation (the gap this fills)

The legacy code reported only a *training-set* confusion matrix. We now score each model on
its **cross-validated out-of-fold predictions** (genuine held-out predictions on decided
players), writing `outputs/metrics/eval_<type>_<season>.yml` + a calibration table.

Metrics (2026 season, 2025 snapshot):

| model    | ROC-AUC | PR-AUC | log-loss | Brier | precision@k | n / positives |
|----------|--------:|-------:|---------:|------:|------------:|--------------:|
| batters  |   0.997 |  0.944 |    0.027 | 0.006 | 0.915 (@71) | 1892 / 71 |
| pitchers |   0.994 |  0.877 |    0.020 | 0.005 | 0.812 (@80) | 5317 / 80 |

PR-AUC is the headline under this severe imbalance (~1–4% positive). Both models are strong
and, per the calibration tables, already well-calibrated at the extremes.

## Modeling options evaluated (Phase 6)

All are config-gated; evaluation drove the defaults. On batters (CV out-of-fold):

- **Class imbalance** (`model.imbalance`): up-sampling *reduced* quality
  (PR-AUC 0.944 → 0.920, log-loss 0.027 → 0.044). The model already handles the
  imbalance, so the default is **`none`**. `up` / `down` / `weights` remain available.
- **Probability calibration** (`model.calibration`): Platt scaling barely moved Brier
  (0.0060 → 0.0058) — the model is already calibrated — so the default is **`none`**.
  `platt` / `isotonic` are available (fit on the out-of-fold predictions).
- **4-class label** (`model.label = multiclass_4`, experimental): predicts
  `FirstBallot | BBWAA | VetCom | No` instead of binary. A multinomial model on the
  relabelled training set scores **macro one-vs-rest AUC 0.989** (FirstBallot 0.996,
  BBWAA 0.991, VetCom 0.970). Writes `outputs/predictions/hof_predictions_<type>s_<season>_4class.csv`
  with per-class probabilities. Binary remains the shipped default.

Reproduce: `Rscript scripts/modeling_experiments.R`.

## BBWAA committee model (`model.committee = bbwaa`)

The Hall is elected by distinct bodies, so the model is committee-specific (`model.committee`).
The **default is `bbwaa`** — the Baseball Writers' vote, the most rule-bound and predictable route.
`any_route` is the original model: induction by *any* body (BBWAA + Veterans/Era committees)
collapsed into one `inducted` label.

**Unit of analysis — the player-ballot-year panel** (`R/ballot_features.R`). One row per actual
BBWAA ballot appearance (1936…`season − 1`), reconstructed from `HallOfFame` (`share =
votes / ballots`). Training on real ballot rows makes BBWAA eligibility self-enforcing (a player
only has rows if they cleared the 10-season / 5-years-retired / 5%-survival / ≤10-year-tenure
rules and were nominated). Per-row ballot-history features are computed **strictly from prior
years** (no leakage): `appearance_number`, `is_first_ballot`, `prev_share`, `prev2_share`,
`prev_delta` (momentum), `max_share_prior`, `years_remaining` (era-aware, `bbwaa_ballot_max_years`),
plus **ballot-strength** features for Rule-of-10 crowding (`competitor_mass` = the rest of the
field's returning vote demand, `n_cand`, `n_strong_competitors`) — a weak/uncrowded ballot lets
borderline candidates separate upward (toggle `bbwaa.use_ballot_strength`; historically the effect
is large post-2014, when the 10-vote cap binds hardest). These join the exact any-route career-merit
matrix (`assemble_* → get_fit_data`) over a full-history window; the any-route `inducted` column is
dropped (it would leak).

**One model, three outputs.** A vote-**share regressor** (`share = votes/ballots`) is trained with
**grouped CV by ballot year** (leave-one-cycle-out) — random k-fold would leak because a player's
adjacent years are autocorrelated through `prev_share`. From that single leak-free model:

| output | how |
|---|---|
| `pred_share` | regressor point prediction for the cycle |
| `P_elected_cycle` | Monte-Carlo: fraction of year-1 simulations reaching 75% |
| `P_eventual_bbwaa` | Monte-Carlo: fraction ever reaching 75% before the ballot expires |

The simulation (`simulate_bbwaa`) rolls each candidate forward year by year — re-feeding each
simulated season's share as next year's `prev_share`, adding noise, and applying the 75% election /
5% survival / era ballot-tenure rules. Two refinements matter:

- **Heteroscedastic noise** (`bbwaa.resid_mode: conditional`) — residuals are drawn from OOF rows
  whose predicted share is *near* the query share. Variance is strongly share-dependent (the model
  nails low-share players but a candidate near 75% has ~2× the spread), so a single global pool
  understated the upside for boundary candidates (a 72% projection gave only ~20% cycle odds vs a
  ~40% empirical rate). `global` restores the old single-pool behaviour.
- **Decaying ballot strength** (`bbwaa.ballot_decay`) — `competitor_mass` shrinks each simulated
  year, modelling the logjam clearing as strong holdovers get elected/expire (raises multi-year
  eventual odds for climbers).

Leak-free (unlike a trajectory classifier, whose features would encode the election-year share) and
makes `P_eventual_bbwaa ≥ P_elected_cycle` by construction. Config: `bbwaa.engine` (default
`xgbTree`), `bbwaa.n_sims`, `bbwaa.elect_pct`, `bbwaa.survive_pct`, `bbwaa.resid_mode`,
`bbwaa.use_ballot_strength`, `bbwaa.ballot_decay`.

**Train / score / outputs.** Train = all panel rows ≤ `label_max_yr`, per player type (routed by
`POS`), minus `cohorts.banned_ids`. Score = the cycle's ballot cohort only (from
`Data/ballots/ballot_<season>.yml`), holdovers carrying their real trajectory and first-timers
defaulting to appearance 1. Writes `outputs/predictions/hof_bbwaa_<type>s_<season>.csv`
(`playerID, name, POS, appearance_number, is_first_ballot, prev_share, pred_share,
P_elected_cycle, P_eventual_bbwaa`), plus `outputs/models/bbwaa_<type>_<season>.rds` and
`outputs/metrics/eval_bbwaa_<type>_<season>.yml` (share MAE/RMSE, election ROC/PR-AUC).

Run: `Rscript run.R` (BBWAA is the default `model.committee`). Set `model.committee: any_route`
for the original all-committees model, which is otherwise untouched.

## Temporal correctness

- Train = players whose HOF fate is settled (`finalGame ≤ season − 15`), plus recent
  inductees. The prediction set is the recent + ballot cohort.
- **Labels are capped at `season − 1`** so the model never trains or is evaluated on the
  results of the season it predicts (the 2025 snapshot's `HallOfFame` already contains the
  2026 class — see `R/config.R`).
- Active players (snapshot `finalGame = NA`) are given an effective last-season so they are
  scored (see `docs/PARITY.md`).
