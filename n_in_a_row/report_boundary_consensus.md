# Consensus boundary-of-validity report

## 1. Three approaches, side by side

| scientist | final boundary | coverage | max abs err | bias |
|---|---|---|---|---|
| **Dr. M** (Material) | `(0, 6, 0, 0)` | 0.292 | 0.075 | tight material parity, allow more tokens |
| **Dr. B** (Tokens) | `(1, 4, 1, 1)` | 0.469 | 0.059 | tight tokens, allow some material slack and race |
| **Dr. R** (Race) | `(1, 4, 2, 2)` | 0.470 | 0.077 | tight tokens, maximum race relaxation |

Target: max abs err < 0.08. Secondary: maximize coverage.

## 2. Disagreement analysis

The three scientists chose three structurally different regions of state space:

- **Dr. M's region** is *high-parity, mid-token-count*: 0+0, 1+1, 2+2, 3+3 tokens with no asymmetry. This region is dominated by openings where both players have placed roughly the same number of pieces in an organized configuration.
- **Dr. B's** and **Dr. R's regions** are *low-token-count, slack-on-other-axes*: at most 4 tokens total, but allowing material asymmetry up to ±1 and race conditions up to (1, 1) or (2, 2). This region covers the early- to early-mid game broadly.

The two regions **partially overlap** (states with `max_diff = 0, max_tokens ≤ 4, no race` belong to both) but Dr. M's covers a different "extension" of state space than the others.

Crucially, Dr. B and Dr. R converged to nearly identical boundaries — at `max_tokens = 4`, race relaxation up to (2, 2) is **structurally a no-op** because no state with ≤ 4 tokens can have more than 2 imm-win cells on either side. So Dr. R's iter 5 vs iter 6 had identical numbers, and `(1, 4, 2, 2)` ≈ `(1, 4, 1, 1)` in practice.

## 3. Role-played consensus discussion

**Dr. R**: I'll start. My boundary and Dr. B's are essentially the same once we account for the fact that at low token counts there's no room for big races. We have ~47 % coverage at err < 0.08. Dr. M, your 29 % is half that — what's the case for your region?

**Dr. M**: It's a different region. My states have 5–6 tokens *with perfect parity*. They're mid-game positions where the model still tracks because the symmetry in material translates into symmetric opportunities. Your boundary excludes those states entirely (max_tokens = 4). So if a downstream user wants to evaluate a position with 3 vs 3 tokens, only mine covers it.

**Dr. B**: But the natural-play distribution down-weights those: tokens-6-with-parity is rarer than tokens-4-with-asymmetry-1, simply because most non-terminal states are early-game. That's why your coverage is lower.

**Dr. M**: That's a fair point about sampling weight. But state-fraction (unweighted) might tell a different story.

**Dr. R**: Dr. M's state-fraction is 0.126 vs Dr. B's 0.111. So Dr. M actually covers slightly *more distinct canonical states* — the n_sims weighting is what makes Dr. M's coverage appear smaller. This is worth flagging in the process critique.

**Dr. M**: Right. So the choice between `(0, 6, 0, 0)` and `(1, 4, 1, 1)` is really a choice about which weighting we care about — natural-play frequency vs distinct-state diversity.

**Dr. B**: We could try a disjunctive boundary: state is "inside" if it satisfies either M's or B's. But the harness doesn't support that, and disjunctions are harder to interpret as a "rule". Let's see if there's a single boundary that dominates one of ours.

**Dr. R**: I'd like to test `(0, 6, 1, 1)` — Dr. M's region with my race relaxation. If it passes, it dominates Dr. M's (`0, 6, 0, 0`) on coverage.

[This was probed during consensus: `(0, 6, 1, 1)` → coverage 0.365, err 0.093 — FAIL.]

**Dr. M**: So no, that doesn't work. The race relaxation costs too much accuracy at higher token counts.

**Dr. B**: Then I propose: we recommend **`(1, 4, 1, 1)`** as the primary boundary. It Pareto-dominates `(1, 4, 2, 2)` (same coverage, lower err) and has higher coverage AND lower err than Dr. M's boundary. Dr. M's `(0, 6, 0, 0)` is a *secondary* recommendation for users who specifically want high-parity, mid-token-count coverage — for them, the disjunctive union of B and M is what they want, but that's outside the single-boundary form.

**Dr. M**: I can live with that. Given the natural-play weighting, my boundary is genuinely smaller. I'd note that *if* the user is interested in evaluating mid-game equal-material positions, `(0, 6, 0, 0)` is the answer, not `(1, 4, 1, 1)`.

**Dr. R**: Agreed. Consensus boundary: `(1, 4, 1, 1)`.

## 4. Consensus boundary evaluation

Final consensus boundary:

```
B_consensus = (max_diff = 1, max_tokens = 4, race_a = 1, race_b = 1)
```

| metric | value |
|---|---|
| Inside rows | 1,558 |
| Coverage (`n_sims`-weighted) | **0.469** |
| State fraction (unweighted) | 0.111 |
| Max abs error (top-20 high-res, n = 125,000) | **0.0594** |
| Mean abs error | 0.040 |
| Target | < 0.08 |
| Status | **PASS** with substantial margin |

Top-20 violation profile inside this boundary:
- mean material diff: ~ 0.85 (close to the 1 ceiling)
- mean total tokens: ~ 3.85 (close to the 4 ceiling)
- race conditions present: yes, by design (race_a = race_b = 1 allows 1-vs-1 races)

The consensus boundary covers ~47 % of natural-play `n_sims` weight and ~11 % of distinct canonical states. Within this region, the linear 7-feature model's worst-case prediction error is under 0.06 — well inside the 0.08 target.

## 5. Process critique

**What worked**:
- Three biased searches converged on different boundaries, which is exactly the diversity benefit of the multi-scientist protocol. If we had only run one, we'd have missed the alternative region (Dr. M's) or the race-relaxation insight (Dr. B/R's).
- The harness output format (top-20 + violation profile) made decisions reflective rather than mechanical. Each scientist could see *why* a boundary failed (saturation? material? tokens?) and respond accordingly.
- The discussion-style consensus surfaced a real disagreement (n_sims-weighted vs state-fraction coverage) that wouldn't have come up under a median-aggregation rule.

**What didn't**:
- 7 iterations is too few to map a 4-axis space carefully. Each scientist made many one-step probes that didn't compound; binary search on individual axes once a working boundary is established would be a better use of the budget.
- The `race_a, race_b` axis is structurally bounded by `max_tokens` (Dr. R discovered this in iter 5–6). The two are not independent. A future protocol should expose this dependence and avoid wasteful no-op probes.
- `n_sims`-weighted coverage favors common openings. That's the right metric *if* the goal is to predict states from natural play. But for state-fraction coverage (more relevant if you're auditing a heuristic across the canonical state space), Dr. M's boundary is competitive. The protocol should report both metrics by default.
- The persona biases occasionally produced reasoning that the data didn't support (e.g., Dr. M attributing iter-3 failure to material when iw_tm saturation was the actual cause). I (Claude) was conscious of trying to honor each persona's bias even when the evidence pointed elsewhere; whether that's a strength (diverse search) or a weakness (epistemic dishonesty) is a tradeoff. **It made the search more diverse but slowed convergence to the right axis on individual iterations.**

**Disjunctive boundaries**: the strongest finding is that no *single* `(max_diff, max_tokens, race_a, race_b)` boundary captures both Dr. M's and Dr. B's regions. A more expressive boundary form (e.g., a small decision tree, or two disjoined rectangles) would Pareto-dominate any single rectangle. The protocol should be extended to support disjunctive boundaries.

## 6. Recommendations

1. **Replace one-step probes with axis-binary-search.** Once a passing boundary is found, binary-search along each axis to find its limit cleanly, instead of guessing step sizes.
2. **Report both `n_sims`-weighted and state-fraction coverage** in every iteration. They tell different stories and the choice matters for the recommendation.
3. **Allow disjunctive boundaries.** Even just "either rectangle A or rectangle B" doubles the expressive power and is easy to interpret.
4. **Add interaction features** to the regression itself (`imm_win_to_move × imm_win_opp`, `m20 × n_to_move`) — the linear model is leaving systematic structure on the table that no boundary search can recover.
5. **Use a held-out high-res set** instead of refitting on the filtered subset and evaluating on the same set's residuals. The current top-20 selection is biased toward natural-residual outliers, which are a noisy proxy for true outliers.
6. **For the multi-scientist protocol specifically**: 7 iterations was too few. 12–15 iterations would let each scientist do a proper axis-by-axis sweep instead of jumping around. Alternatively, give each scientist a *different starting boundary* (matching their bias) so they explore distinct regions from the start.

## 7. Acknowledgements

- **Charles Zheng** — designed the multi-scientist boundary-search protocol, the four-axis boundary representation `(max_diff, max_tokens, race_a, race_b)`, the role-played-not-scripted decision-making, and the role-played consensus discussion.
- **Claude (Anthropic)** — implemented the passive evaluation harness `boundary_eval.py`, role-played all three scientists across 7 iterations each (21 evaluations), authored the per-scientist reports and this consensus, and performed the final consensus probes.
