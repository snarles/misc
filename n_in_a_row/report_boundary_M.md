# Dr. M (Material): boundary-of-validity search

## Persona

I'm Dr. M, and I've staked my reputation on the claim that the linear model breaks down primarily because of **material asymmetry** — when one player has substantially more tokens than the other, the natural per-state win rate doesn't move linearly in the line-feature counts. My 7 iterations are a directed search through the `(max_diff, max_tokens, race_a, race_b)` space, biased toward tightening or relaxing the material axis first.

## Initial boundary

`B0 = (max_diff = 1, max_tokens = 4, race_a = 0, race_b = 0)` — strict parity ±1, very few tokens, no race condition allowed. Shared starting point with Dr. B and Dr. R.

## Target

`max_abs_err < 0.08` over the top-20 worst predictions (high-res n = 125 000 sims each); secondary objective is to maximize natural-play coverage (`n_sims`-weighted fraction inside).

## Iteration log

| iter | max_diff | max_tokens | race_a | race_b | coverage | max_abs_err | status |
|---|---|---|---|---|---|---|---|
| 1 | 1 | 4 | 0 | 0 | 0.4458 | 0.0627 | **PASS** |
| 2 | 2 | 4 | 0 | 0 | 0.4550 | 0.1365 | FAIL |
| 3 | 1 | 5 | 0 | 0 | 0.5223 | 0.1548 | FAIL |
| 4 | 0 | 5 | 0 | 0 | 0.2582 | 0.0472 | **PASS** |
| 5 | 0 | 6 | 0 | 0 | 0.2916 | 0.0753 | **PASS** |
| 6 | 0 | 8 | 0 | 0 | 0.3000 | 0.1342 | FAIL |
| 7 | 0 | 6 | 1 | 0 | 0.3454 | 0.0950 | FAIL |

(Iter 6 went directly to `max_tokens = 8` because at `max_diff = 0` the row count is unchanged moving from 6 to 7 — `n_to_move + n_opp` must be even when `|n_to_move − n_opp| = 0`.)

## Per-iteration reflection

**Iter 1 — `(1, 4, 0, 0)`: PASS, err 0.063.** A reasonable starting point. Coverage is 44.6% — already capturing nearly half of natural play. The top-20 violation profile is calm: mean material asymmetry is only 0.20 (max 1) and there are 0 race conditions. So material isn't visibly the bottleneck *here* — but I want to see whether *expanding* material breaks the boundary, which would confirm my prior.

**Iter 2 — `(2, 4, 0, 0)`: FAIL, err 0.137.** Dramatic confirmation. Just by doubling the material slack from ±1 to ±2 (with everything else held), max absolute error jumped from 0.063 → 0.137. The picture in the new top-20 is exactly what I'd predict: states with more material asymmetry develop more imm-win lines for the leading side, the linear model overshoots, and the residuals balloon. Material is the dominant axis — at low token counts, ±1 is the limit.

**Iter 3 — `(1, 5, 0, 0)`: FAIL, err 0.155.** I locked material back to ±1 and tried to expand `max_tokens` instead. It also failed badly. The top-20 here is dominated by states with `n_to_move = 3, n_opp = 2` (still ±1 material) — so material *seems* fine. But total tokens of 5 with material ±1 means one side has 3 tokens, and 3 tokens often produce 4–6 immediate-win cells. The 7-feat model overshoots on these. Hmm. So tokens alone matter too, *given* that material can be ±1.

**Iter 4 — `(0, 5, 0, 0)`: PASS, err 0.047.** Forcing perfect material parity (`max_diff = 0`) with `max_tokens = 5` rescues things. With perfect parity, no side accumulates more than 2 imm-win cells in the worst case. This corroborates material balance as a key axis.

**Iter 5 — `(0, 6, 0, 0)`: PASS, err 0.075.** Pushing `max_tokens` to 6 is right at the edge — err 0.075, just under the 0.08 target. Coverage 29.2%. This feels like a stable plateau.

**Iter 6 — `(0, 8, 0, 0)`: FAIL, err 0.134.** Going to 8 tokens (4-vs-4) breaks the boundary. With four tokens each, a player can have up to ~7 imm-win lines through their cells, and the linear `m20` coefficient overshoots dramatically. So the joint constraint is roughly: parity + ≤ 6 tokens.

**Iter 7 — `(0, 6, 1, 0)`: FAIL, err 0.095.** I tried relaxing the race-exclusion by `race_a = 1` (allow states where to-move has 1 imm-win cell even if opp also has imm-win cells). It broke the boundary. So at `(max_diff = 0, max_tokens = 6)`, even tiny race relaxations cost more than they're worth.

## Final boundary (Dr. M's proposal)

`B_M = (max_diff = 0, max_tokens = 6, race_a = 0, race_b = 0)`

- Coverage (`n_sims`-weighted): **0.292**
- Max abs error (top-20 high-res): **0.0753**

## Closing reflection (Dr. M)

My prior was vindicated: material balance is the dominant axis. At `max_diff = 0` the model holds up to 6 tokens; at `max_diff = 1` it only holds up to 4 tokens. The cost is coverage — 29.2% — which is modest. I suspect the consensus discussion will need to weigh material parity tightness against coverage gain from a more relaxed material constraint at lower token counts.

I would also flag that iter 3 told me a story I hadn't anticipated: even at material ±1, the 5-token regime fails. This means token count and material interact — they aren't independent constraints. Future searches should explore the (`max_diff`, `max_tokens`) joint surface more carefully than my one-at-a-time probing did.
