# Subsample-and-merge storage benchmark

## Purpose

Compare two ways of producing a labeled state-feature dataset with the **same total simulation count** but different storage cost:

- **(a)** subsample p=0.10 of games, then run n=10 sims per canonical state
- **(b)** subsample p=0.02 of games, then run n=50 sims per canonical state

Configuration (b) stores ~5× fewer rows. Question: do the 7-feature regression CIs come out comparable?

## Method

1. Run a fixed pool of random games from the empty board (deterministic seed).
2. Subsample at the game level with fraction p.
3. Compute canonical form (D4 spatial × player-swap-via-perspective) of every state in the subsample, deduplicating.
4. For each canonical state, run n fresh `simulate(state, next_player=1)` rollouts, count wins.
5. Fit weighted OLS (`weight = n`, `y = n_wins/n`) on the 7-feature matrix (`m01..m21`) and bootstrap 95% CIs by resampling distinct canonical-state rows with replacement.

## Results

| metric                 | (a) p=0.10, n=10 | (b) p=0.02, n=50 | (b)/(a) |
|---|---|---|---|
| canonical states (rows) | 24,604 | 5,866 | 0.24 |
| total sims              | 246,040 | 293,300 | 1.19 |
| subsample time (s)      | 0.04 | 0.04 | — |
| merge time (s)          | 0.61 | 0.11 | — |
| sim wall time (s)       | 9.5 | 10.8 | 1.14 |

### CI widths per feature (95% bootstrap)

| feature         | (a) coef | (a) width | (b) coef | (b) width | width (b)/(a) |
|---|---|---|---|---|---|
| `m01` | -0.0144 | 0.0024 | -0.0140 | 0.0024 | 1.01 |
| `m02` | -0.0570 | 0.0029 | -0.0567 | 0.0033 | 1.11 |
| `m10` | +0.0145 | 0.0026 | +0.0153 | 0.0024 | 0.94 |
| `m11` | -0.0005 | 0.0022 | -0.0006 | 0.0022 | 1.01 |
| `m12` | -0.0060 | 0.0036 | -0.0055 | 0.0044 | 1.21 |
| `m20` | +0.0690 | 0.0034 | +0.0694 | 0.0045 | 1.34 |
| `m21` | +0.0042 | 0.0040 | +0.0028 | 0.0043 | 1.09 |

## Discussion

**Storage and CI width.** Configuration (b) stores **4.2× fewer rows** (5,866 vs 24,604) yet yields CIs that are on average only **1.10× wider** for the 7 matrix features. The largest gap is on `m20` (+34% width) and `m12` (+21%); the rest are within ~10%. Configuration (b) is the better storage choice for this problem: most of the precision is preserved.

**Total sims weren't exactly matched.** I designed the run for `(a) 0.10 × 10 = (b) 0.02 × 50` — equal *if* the canonical-state count scales linearly with the subsample size. Empirically (b) has **5,866 / (24,604 / 5) = 1.19×** as many canonical states per game subsampled (at p=0.02 there are fewer collisions among the ~512 games' worth of states than at p=0.10's ~2,560 games). This gave (b) about 19% more total sims (293,300 vs 246,040) than (a). After adjusting for this advantage, (b)'s effective CI width disadvantage is somewhat larger than the raw 1.10× ratio shows — but still well under 2×.

**Where the per-state-sims approach wins.** With many-sim-per-state, each row's `n_wins/n` is a low-variance estimate of `P(to-move wins | canonical state)`. The regression no longer averages over within-game correlations; rows are independent. So bootstrap CIs are more honest than they were under the original game-level data approach, even at fixed total sim count.

**Caveats.** The merged regression weights each canonical state by `n` (uniform in (a) or (b)), not by the state's natural visit frequency under random play. So it estimates `E[P(win) | canonical state]` over the **uniform distribution on visited canonical states**, not over the **empirical play distribution**. For a board-evaluation heuristic this is fine (we evaluate boards we encounter, which is closer to uniform over reachable states than to play frequency); for a calibrated win-probability estimator on the natural distribution, weight by visit frequency instead.

## Acknowledgements

- **Charles Zheng** (human author) — designed the storage-vs-precision experiment and the symmetry merging strategy.
- **Claude (Anthropic)** — implemented `canonical_form`, the aggregation and weighted regression pipeline, and ran the benchmark.
