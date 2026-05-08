# Dr. R (Race / tempo): boundary-of-validity search

## Persona

I'm Dr. R. My thesis is that the dominant non-linearity in the linear win-probability model is the **race condition** — the structural fact that whoever fires first this ply wins, which is fundamentally non-additive in the linear feature counts. I expect race-relaxation (i.e., letting both sides have immediate-win cells) to be the most restrictive axis. My iterations probe `(race_a, race_b)` first.

## Initial boundary

`B0 = (max_diff = 1, max_tokens = 4, race_a = 0, race_b = 0)`. Same starting point as Dr. M and Dr. B.

## Iteration log

| iter | max_diff | max_tokens | race_a | race_b | coverage | max_abs_err | status |
|---|---|---|---|---|---|---|---|
| 1 | 1 | 4 | 0 | 0 | 0.4458 | 0.0627 | **PASS** |
| 2 | 1 | 4 | 1 | 0 | 0.4664 | 0.0590 | **PASS** |
| 3 | 1 | 4 | 0 | 1 | 0.4671 | 0.0605 | **PASS** |
| 4 | 1 | 4 | 1 | 1 | 0.4693 | 0.0594 | **PASS** |
| 5 | 1 | 4 | 2 | 2 | 0.4699 | 0.0766 | **PASS** |
| 6 | 1 | 4 | 5 | 5 | 0.4699 | 0.0766 | **PASS** |
| 7 | 1 | 5 | 2 | 2 | 0.5896 | 0.1683 | FAIL |

## Per-iteration reflection

**Iter 1 — `(1, 4, 0, 0)`: PASS, err 0.063.** Baseline. Race exclusion at the strictest setting. Top-20 violation profile shows 0/20 race conditions — by construction. As Dr. R, I'm going to systematically open up the race axis to see how restrictive it really is.

**Iter 2 — `(1, 4, 1, 0)`: PASS, err 0.059.** Allowing `imm_win_to_move` up to 1 (with any `imm_win_opp`) actually *improves* the max abs err from 0.063 to 0.059 — and lifts coverage from 44.6% to 46.6%. So one-sided race relaxation in to-move's favor is free. Small ply-asymmetry favoring the player who moves first.

**Iter 3 — `(1, 4, 0, 1)`: PASS, err 0.061.** The mirror: allowing `imm_win_opp` up to 1 instead. Nearly identical metrics to iter 2 (the boundary is roughly symmetric in the to-move POV when we keep material balanced). So the marginal race relaxation on either side is benign at low tokens.

**Iter 4 — `(1, 4, 1, 1)`: PASS, err 0.059.** Symmetric race relaxation up to (1, 1). Best coverage so far (46.9%) at a tight error of 0.059. Race conditions involving 1-vs-1 immediate threats are not the bottleneck I expected them to be.

**Iter 5 — `(1, 4, 2, 2)`: PASS, err 0.077.** Pushing race relaxation further to (2, 2). Coverage barely moves to 47.0%; max abs err climbs to 0.077 — close to the 0.08 ceiling. So 2-vs-2 races are present and they cost ~0.018 in error.

**Iter 6 — `(1, 4, 5, 5)`: PASS, err 0.077.** Identical numbers to iter 5! At `max_tokens = 4`, no state actually has more than 2 imm-win cells on either side — there aren't enough tokens to fit. So race relaxation maxes out at (2, 2) at this token level. **The boundary is structurally bounded by tokens**, not just by what I set in the race axis.

**Iter 7 — `(1, 5, 2, 2)`: FAIL, err 0.168.** I tried pairing race relaxation with token expansion. It immediately breaks. Coverage jumped to 58.9% but accuracy is gone. So: race-relaxation alone is fine at low tokens; combining race relaxation with token expansion is not.

## Final boundary (Dr. R's proposal)

`B_R = (max_diff = 1, max_tokens = 4, race_a = 2, race_b = 2)`

- Coverage: **0.470**
- Max abs error: **0.077**

I propose the (2, 2) race-relaxation since it's the maximum that's structurally meaningful at `max_tokens = 4` (anything more is a no-op at this token count). This gives modest extra coverage versus (1, 1) but the error is closer to the ceiling.

If anyone in the consensus meeting prefers a more conservative race policy with safer error, `(1, 4, 1, 1)` (err 0.059) is essentially equivalent on coverage and gives more headroom.

## Closing reflection (Dr. R)

I came in expecting that race conditions would be the dominant limiting factor. **The data refuted my prior at low token counts.** At `max_tokens = 4`, race relaxation up to (2, 2) is essentially free; the boundary is bounded by token count, not by the race axis. Where my hypothesis would have been right is at higher token counts (iter 7), where race + tokens together break the boundary — but I can't separately attribute that to "race causing the problem" vs "token count causing the problem". I'd need to hold tokens at 5 with race fully strict to test this — that overlaps with Dr. M's iter 3, which also failed. So the failure mode is plausibly the conjunction.

Coverage-wise, Dr. B and I converged on roughly the same boundary. We should compare with Dr. M's — they sacrificed coverage to chase strict material parity.
