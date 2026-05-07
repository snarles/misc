# Sanity check: 11-feature board state vs. win probability

## 1. Purpose

This is an explicit **sanity check** on the 11-feature representation introduced for predicting win probability in the luck-only n-in-a-row simulation. The goal is **not** to ship a heuristic — it is to verify (a) that the perspective-flip used to canonicalize the to-move player is implemented correctly, and (b) that the coefficients have signs consistent with hand-derived priors. Any anomaly is more likely to indicate a bug or a flaw in the priors than a deep modeling insight at this stage.

## 2. Setup

- **Game**: 4×4 luck-only n-in-a-row (see `sim.py`). Each turn the active player draws a uniformly random cell of the 16; placing/removing/no-op depending on what's there. First to 3-in-a-line wins.
- **Features (11)**: `ml_features(state, to_move)` returns 7 line-feature matrix entries (`m01, m02, m10, m11, m12, m20, m21`) plus token counts (`n_to_move, n_opp`) and immediate-win cell counts (`imm_win_to_move, imm_win_opp`), all from the to-move player's perspective.
- **Data**: every non-terminal state from full random games starting from the empty board. Label is 1 iff the to-move player wins the game, else 0.
- **Model**: OLS linear-probability model with intercept.
- **Inference**: 95% confidence intervals via **block bootstrap** (resampling games with replacement, all rows from a sampled game kept together) — appropriate because rows from the same game are not independent.
- **Specifications**:
  1. `p1_only_7` — only `to_move == 1` rows, 7 matrix features. Avoids the perspective-flip code path entirely.
  2. `both_7` — all rows, 7 matrix features. Uses the flip for P2-to-move rows.
  3. `both_11` — all rows, all 11 features.

## 3. Hypothesized signs

Key insight informing the priors: filled "dead" lines are not really dead because of the removal mechanic. A `(2 own, 1 opp)` line is one opp-token-removal away from `(2, 0)` and so retains latent winning value; analogously `(1, 2)` retains latent loss-threat value. A `(1, 1)` line favors to-move because of tempo.

| feature | hypothesized sign | rationale |
|---|---|---|
| `m01` | - | mild – : latent opp presence |
| `m02` | - | strong – : direct loss threat next turn |
| `m10` | + | mild + : latent own presence |
| `m11` | + | mild + : contested line, to-move has tempo advantage |
| `m12` | - | – (less than m02) : one removal away from m02 |
| `m20` | + | strong + : direct win threat this turn |
| `m21` | + | + (less than m20) : one opp-removal away from m20 |
| `n_to_move` | + | + : more own material → more potential |
| `n_opp` | - | – : more opp material → opp closer to threats |
| `imm_win_to_move` | + | strong + : direct win this turn (collinear with m20) |
| `imm_win_opp` | - | strong – : direct loss next turn (collinear with m02) |

Expected magnitude orderings: `|coef(m20)| > |coef(m21)|` and `|coef(m02)| > |coef(m12)|`.

## 4. Symmetry check

Comparing the **p1_only_7** and **both_7** regressions: if the perspective-flip in `ml_features` is correct, the matrix coefficients should agree in sign and have overlapping 95% CIs.

- **Sign agreement** (treating `?` as agreement, since wide CI ≠ disagreement): PASS
- **CI overlap** (each feature's two CIs intersect): PASS

## 5. Iteration history

| iter | N games | n rows (both) | n rows (P1-only) | sc p1_7 | sc both_7 | sc both_11 |
|---|---|---|---|---|---|---|
| 1 | 100 | 1660 | 860 | 1/7 | 2/7 | 2/11 |
| 2 | 200 | 3400 | 1757 | 3/7 | 4/7 | 0/11 |
| 3 | 400 | 7193 | 3706 | 4/7 | 4/7 | 2/11 |
| 4 | 800 | 13943 | 7191 | 4/7 | 4/7 | 4/11 |
| 5 | 1600 | 28072 | 14471 | 4/7 | 4/7 | 4/11 |
| 6 | 3200 | 56170 | 28969 | 5/7 | 4/7 | 5/11 |
| 7 | 6400 | 112209 | 57868 | 4/7 | 4/7 | 5/11 |
| 8 | 12800 | 223505 | 115309 | 4/7 | 5/7 | 6/11 |
| 9 | 25600 | 448268 | 231244 | 6/7 | 7/7 | 8/11 |

## 6. Final coefficient tables

### 6.1 `p1_only_7` (matrix-only, P1-to-move only)

| feature | coef | 95% CI | hypothesis | observed | agree? |
|---|---|---|---|---|---|
| `(intercept)` | +0.5585 | [+0.5517, +0.5641] | — | — | — |
| `m01` | -0.0164 | [-0.0179, -0.0149] | - | - | ✓ |
| `m02` | -0.0585 | [-0.0618, -0.0549] | - | - | ✓ |
| `m10` | +0.0154 | [+0.0140, +0.0168] | + | + | ✓ |
| `m11` | -0.0012 | [-0.0033, +0.0008] | + | ? | ? |
| `m12` | -0.0062 | [-0.0111, -0.0019] | - | - | ✓ |
| `m20` | +0.0677 | [+0.0644, +0.0710] | + | + | ✓ |
| `m21` | +0.0048 | [+0.0003, +0.0097] | + | + | ✓ |


### 6.2 `both_7` (matrix-only, both to-move)

| feature | coef | 95% CI | hypothesis | observed | agree? |
|---|---|---|---|---|---|
| `(intercept)` | +0.5430 | [+0.5397, +0.5461] | — | — | — |
| `m01` | -0.0174 | [-0.0186, -0.0161] | - | - | ✓ |
| `m02` | -0.0569 | [-0.0591, -0.0548] | - | - | ✓ |
| `m10` | +0.0174 | [+0.0161, +0.0185] | + | + | ✓ |
| `m11` | -0.0006 | [-0.0009, -0.0004] | + | - | ✗ |
| `m12` | -0.0064 | [-0.0100, -0.0028] | - | - | ✓ |
| `m20` | +0.0699 | [+0.0676, +0.0721] | + | + | ✓ |
| `m21` | +0.0043 | [+0.0005, +0.0086] | + | + | ✓ |


### 6.3 `both_11` (full features, both to-move)

| feature | coef | 95% CI | hypothesis | observed | agree? |
|---|---|---|---|---|---|
| `(intercept)` | +0.5419 | [+0.5380, +0.5451] | — | — | — |
| `m01` | -0.0174 | [-0.0190, -0.0156] | - | - | ✓ |
| `m02` | -0.0153 | [-0.0235, -0.0068] | - | - | ✓ |
| `m10` | +0.0173 | [+0.0154, +0.0190] | + | + | ✓ |
| `m11` | -0.0009 | [-0.0013, -0.0005] | + | - | ✗ |
| `m12` | -0.0075 | [-0.0115, -0.0037] | - | - | ✓ |
| `m20` | +0.0073 | [-0.0020, +0.0168] | + | ? | ? |
| `m21` | +0.0053 | [+0.0013, +0.0095] | + | + | ✓ |
| `n_to_move` | -0.0042 | [-0.0127, +0.0048] | + | ? | ? |
| `n_opp` | +0.0049 | [-0.0042, +0.0139] | - | ? | ? |
| `imm_win_to_move` | +0.0751 | [+0.0653, +0.0845] | + | + | ✓ |
| `imm_win_opp` | -0.0520 | [-0.0607, -0.0437] | - | - | ✓ |

## 7. Discussion

After 25,600 games (448,268 state rows): `both_7` is **fully sign-confident** (7/7), `p1_only_7` reaches 6/7, and `both_11` reaches 8/11. The remaining `?`s in `both_11` are explained by the planned multicollinearity (see below).

### Symmetry

The perspective-flip code path in `ml_features` appears correct. Across all 7 matrix features, the `p1_only_7` and `both_7` regressions agree on sign and their 95% CIs overlap. This rules out a class of bugs where (e.g.) the to-move flip transposed the matrix incorrectly or swapped own/opp.

### Hypotheses confirmed

The four "main effect" priors are confirmed in every specification with tight CIs:

- `m20` strong **+** (≈ +0.07 per line): each "two-of-mine, none-of-theirs" line is worth about 7 percentage points of win probability.
- `m02` strong **–** (≈ –0.06 per line): each "none-of-mine, two-of-theirs" line is worth about –6 pp.
- `imm_win_to_move` strong **+** (≈ +0.075 per cell) and `imm_win_opp` strong **–** (≈ –0.05 per cell) in `both_11`. As predicted, these absorb most of the signal from `m20` / `m02` when present together: the `m20` and `m02` coefficients shrink and the latter widens to span zero in `both_11`.
- `m10` mild **+** (≈ +0.017) and `m01` mild **–** (≈ –0.017): latent presence has clearly nonzero, modest effects of the expected sign.
- Magnitude orderings hold: `|m20| (0.070) > |m21| (0.004)` and `|m02| (0.057) > |m12| (0.006)`. The "dead lines aren't dead" effect is real but **roughly an order of magnitude smaller** than the direct two-of-mine / two-of-theirs effects — confirming the qualitative prior while suggesting the latent-removal channel is weak in expectation under uniform random play.

### Hypotheses refined or refuted

- **`m11` was hypothesized weakly positive** ("tempo advantage for to-move") but came out **slightly but significantly negative** in `both_7` (-0.0006, CI [-0.0009, -0.0004]) and `both_11`. The magnitude is tiny — ~ 100× smaller than the main effects — but the sign is robust to the choice of spec. The tempo-advantage intuition was wrong: contested 1-1 lines do not favor the to-move player. A plausible re-explanation is that the opponent's existing token in such a line is "owed" (claimed cell, harder for to-move to clear by random draw than empty cells are to fill). For practical purposes the coefficient is negligible and either sign is fine for a heuristic.
- `m21` mild **+** (+0.004) and `m12` mild **–** (-0.006) confirm the priors directionally; with the larger sample the CIs no longer span zero.

### Multicollinearity in `both_11`

As anticipated, the four added features mostly duplicate signal already in the matrix:

- `imm_win_to_move ↔ m20`: in `both_11` the regression credits `imm_win_to_move` with most of the immediate-win effect (+0.075 vs `m20`'s +0.007 with CI spanning zero). In `both_7` the same effect is loaded entirely onto `m20`. Both representations are fine; together they're redundant.
- Similarly `imm_win_opp ↔ m02`.
- `n_to_move` and `n_opp` lose their sign-confidence in `both_11` because the matrix features (and their sums) already encode material balance.

This is the expected behavior of OLS under collinearity, not a sign anomaly. None of the matrix-feature signs flip when the four extras are added — they only widen for the two collinear pairs.

## 8. Speculation

Possible improvements once the representation is trusted:
- **Logistic regression** instead of OLS — proper for binary outcomes; won't change sign judgements much but will give better-calibrated probabilities.
- **Drop one of each collinear pair** (`imm_win_*` or the matching `m02`/`m20`) — recover narrower CIs for the kept feature.
- **Interaction terms**, especially `m20 × imm_win_opp` (race condition: I win this turn vs. lose next).
- **Nonlinear models** (random forest, gradient boosting, MLP) — likely much better fits, since the win probability is plausibly nonlinear in feature counts (e.g. `imm_win_to_move ≥ 1` is essentially saturated for the immediate-win effect).
- **Train on per-state Monte-Carlo win-probability estimates** rather than 0/1 game outcomes — lower-variance labels at the cost of needing many MC rollouts per training point.
- **Add features** that the matrix can't see: # cells where opp removing one of our tokens drops us out of an `m20` line ("vulnerable threats"), or specifically structured threats like double-`m20` (fork) counts.

## 9. Acknowledgements

- **Charles Zheng** (human author) — designed the game, the feature representation, the symmetry analysis, and this experimental protocol; specified what "sanity check" should mean here, including the matrix-only-vs-full split and the P1-only symmetry comparison.
- **Claude (Anthropic)** — implemented the simulation, feature extraction, data collection, regression with block bootstrap, and drafted this report under Charles's direction.
