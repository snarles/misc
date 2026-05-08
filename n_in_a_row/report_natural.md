# Natural-play-distribution dataset: regression report

## 1. Purpose

Train a weighted linear-probability model on a merged dataset whose row weights match the **natural play distribution**: for each game from the empty board, one non-terminal state is sampled uniformly from its trajectory, and per-canonical-state simulation counts are proportional to the natural visit frequency. The regression estimates `E[P(to-move wins) | features]` over the distribution of states a player would actually encounter while playing — the right target for a board-evaluation heuristic.

Two specifications are reported side-by-side:

- **7-feature (matrix only)** — the canonical line-feature counts `m01..m21`.
- **11-feature (full)** — adds the four highly collinear features `n_to_move, n_opp, imm_win_to_move, imm_win_opp`.

The matrix coefficient signs should be stable across the two specifications. Wider CIs in the 11-feature run for the collinear pairs (`m20 ↔ imm_win_to_move`, `m02 ↔ imm_win_opp`) are expected.

## 2. Method

- **Sampling**: `G = 25,000` games played from the empty board (`seed_games=0`); from each, one non-terminal state sampled uniformly from `history[0..turns-1]`. The game outcome is discarded.

- **Merging**: by `canonical_form` (D4 spatial × player-swap-via-perspective). `nat_count[X]` = number of game-samples that landed in canonical bucket X.

- **Per-state sims**: `n_sims_X = k × nat_count_X`, with `k = 500` (`seed_sims=1`). Each sim is `simulate(state, next_player=1)`; we count `n_wins_X` as the number of sims where the to-move player wins.

- **Regression**: weighted OLS with `weight = n_sims`, `y = n_wins / n_sims`, intercept included. 95% CIs from a row-level bootstrap (1000 resamples; resampling distinct canonical rows with replacement, weights carried). Sims are independent across rows so block bootstrap is not needed here.

## 3. Dataset summary

| metric | value |
|---|---|
| Games (G) | 25,000 |
| Sims-per-sample (k) | 500 |
| Canonical states (rows) | 14,060 |
| Max `nat_count` | 1,989 |
| Total sims | 12,500,000 |
| TSV file size | 547 KB |

Heavy merging in the early game pulls samples from `(,)` — the empty-board sample alone accounts for roughly `G / avg_game_length ≈ 25000/17 ≈ 1470` of the 25,000 samples; everything else has smaller `nat_count`.

## 4. 7-feature regression (matrix-only)

| feature | coef | 95% CI | width | hypothesis | observed | agree? |
|---|---|---|---|---|---|---|
| `(intercept)` | +0.5426 | [+0.5184, +0.5533] | 0.0349 | — | — | — |
| `m01` | -0.0173 | [-0.0186, -0.0148] | 0.0038 | - | - | ✓ |
| `m02` | -0.0594 | [-0.0604, -0.0584] | 0.0020 | - | - | ✓ |
| `m10` | +0.0175 | [+0.0157, +0.0193] | 0.0036 | + | + | ✓ |
| `m11` | -0.0004 | [-0.0012, +0.0005] | 0.0017 | + | ? | ? |
| `m12` | -0.0054 | [-0.0064, -0.0045] | 0.0020 | - | - | ✓ |
| `m20` | +0.0723 | [+0.0713, +0.0735] | 0.0022 | + | + | ✓ |
| `m21` | +0.0031 | [+0.0021, +0.0045] | 0.0024 | + | + | ✓ |


**Sign-confident: 6/7 features.**

**Magnitude orderings (point estimates):** `|m20| = 0.0723` vs `|m21| = 0.0031` (✓); `|m02| = 0.0594` vs `|m12| = 0.0054` (✓).

## 5. 11-feature regression (full)

| feature | coef | 95% CI | width | hypothesis | observed | agree? |
|---|---|---|---|---|---|---|
| `(intercept)` | +0.5431 | [+0.5184, +0.5538] | 0.0355 | — | — | — |
| `m01` | -0.0164 | [-0.0180, -0.0138] | 0.0042 | - | - | ✓ |
| `m02` | -0.0143 | [-0.0176, -0.0111] | 0.0066 | - | - | ✓ |
| `m10` | +0.0163 | [+0.0150, +0.0180] | 0.0029 | + | + | ✓ |
| `m11` | -0.0012 | [-0.0024, -0.0000] | 0.0024 | + | - | ✗ |
| `m12` | -0.0052 | [-0.0073, -0.0033] | 0.0040 | - | - | ✓ |
| `m20` | +0.0061 | [+0.0029, +0.0100] | 0.0071 | + | + | ✓ |
| `m21` | +0.0022 | [-0.0002, +0.0047] | 0.0050 | + | ? | ? |
| `n_to_move` | +0.0062 | [+0.0017, +0.0095] | 0.0078 | + | + | ✓ |
| `n_opp` | -0.0042 | [-0.0078, +0.0008] | 0.0086 | - | ? | ? |
| `imm_win_to_move` | +0.0730 | [+0.0697, +0.0757] | 0.0060 | + | + | ✓ |
| `imm_win_opp` | -0.0512 | [-0.0543, -0.0484] | 0.0059 | - | - | ✓ |


**Sign-confident: 9/11 features.**

**Matrix-feature sign stability** (7-feature → 11-feature):

| feature | sign in 7-feat | sign in 11-feat | stable? |
|---|---|---|---|
| `m01` | - | - | ✓ |
| `m02` | - | - | ✓ |
| `m10` | + | + | ✓ |
| `m11` | ? | - | ✓ |
| `m12` | - | - | ✓ |
| `m20` | + | + | ✓ |
| `m21` | + | ? | ✓ |

**Multicollinearity:** as expected, `m20` (+0.0061) and `imm_win_to_move` (+0.0730) absorb each other in the 11-feature spec; same for `m02` (-0.0143) and `imm_win_opp` (-0.0512). Combined effect (per row, across the pair) is preserved across specifications even when each individual coefficient widens.

## 6. CI-width comparison vs uniform-weighted configs

Compares the natural-distribution 7-feature CI widths against the earlier `merged_a` and `merged_b` (uniform-weighted) results from `report_aggregation.md`.

| feature | merged_a width | merged_b width | merged_natural width (7-feat) | shrink vs (a) | shrink vs (b) |
|---|---|---|---|---|---|
| `m01` | 0.0024 | 0.0024 | 0.0038 | 0.63× | 0.63× |
| `m02` | 0.0029 | 0.0033 | 0.0020 | 1.45× | 1.65× |
| `m10` | 0.0026 | 0.0024 | 0.0036 | 0.72× | 0.67× |
| `m11` | 0.0022 | 0.0022 | 0.0017 | 1.27× | 1.27× |
| `m12` | 0.0036 | 0.0044 | 0.0020 | 1.84× | 2.25× |
| `m20` | 0.0034 | 0.0045 | 0.0022 | 1.55× | 2.05× |
| `m21` | 0.0040 | 0.0043 | 0.0024 | 1.68× | 1.80× |

**Average CI-width shrink:** **1.31× vs merged_a**, **1.47× vs merged_b**.

## 7. Discussion

The natural-distribution dataset achieves a 1.3× CI-width shrink vs `merged_a`. The improvement is real but smaller than `√(total_sims_natural / total_sims_a) ≈ 7×`, which suggests the CI is bottlenecked by rare-but-feature-distinct states that even the natural-distribution approach cannot densely cover.

**Hypothesis confirmation summary** (7-feature spec). 
Confirmed: `m01`, `m02`, `m10`, `m12`, `m20`, `m21`. Refuted: (none). Ambiguous: `m11`.

**Interpretation of natural weighting.** Each row's residual contributes to the loss in proportion to `n_sims`, which is `k × nat_count`. Common openings (small board, low-feature-magnitude states) get the most weight; rare end-game configurations get little. The fitted coefficients are therefore more representative of mid-game decision points than of unusual late-game positions.

## 8. Acknowledgements

- **Charles Zheng** (human author) — designed the natural-play sampling protocol (one random non-terminal state per game), the merged-count-as-multiplier scheme, and the storage/time budget constraints.
- **Claude (Anthropic)** — implemented `collect_natural`, the weighted regression with bootstrap, and the post-hoc 7- vs 11-feature comparison reported here.
