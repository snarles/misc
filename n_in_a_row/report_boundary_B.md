# Dr. B (Board density / tokens): boundary-of-validity search

## Persona

I'm Dr. B. My thesis is that the linear model fails because of **board complexity that grows with the total number of tokens**: with more tokens, the removal mechanic creates branching that no static feature vector can represent. My iterations probe `max_tokens` first, and when failures appear, I look at whether the failures correlate with token density before attributing them to anything else.

## Initial boundary

`B0 = (max_diff = 1, max_tokens = 4, race_a = 0, race_b = 0)`. Same starting point as Dr. M and Dr. R.

## Iteration log

| iter | max_diff | max_tokens | race_a | race_b | coverage | max_abs_err | status |
|---|---|---|---|---|---|---|---|
| 1 | 1 | 4 | 0 | 0 | 0.4458 | 0.0627 | **PASS** |
| 2 | 1 | 6 | 0 | 0 | 0.5557 | 0.1523 | FAIL |
| 3 | 0 | 6 | 0 | 0 | 0.2916 | 0.0753 | **PASS** |
| 4 | 0 | 10 | 0 | 0 | 0.3009 | 0.1323 | FAIL |
| 5 | 1 | 4 | 1 | 1 | 0.4693 | 0.0594 | **PASS** |
| 6 | 1 | 5 | 1 | 1 | 0.5862 | 0.1651 | FAIL |
| 7 | 0 | 5 | 1 | 0 | 0.2788 | 0.0366 | **PASS** |

## Per-iteration reflection

**Iter 1 — `(1, 4, 0, 0)`: PASS, err 0.063.** Baseline. Coverage 44.6%; that's already meaningful coverage for a tight boundary. As Dr. B, I notice that the top-20 has mean total tokens 3.80 (max 4 — the boundary), so we're already near the ceiling on my axis. Pushing tokens is the obvious direction.

**Iter 2 — `(1, 6, 0, 0)`: FAIL, err 0.152.** Doubling the token slack catastrophically breaks the boundary. Coverage went up to 55.6% but accuracy is gone. The top-20 here sits at the new token ceiling (5–6 tokens), with iw_tm = 4–6 in many cases. As Dr. B, this is exactly what I'd predict — once you hit 5+ tokens, the linear model has no way to express the saturating "I have many ways to win" effect.

**Iter 3 — `(0, 6, 0, 0)`: PASS, err 0.075.** Forcing material parity at `max_tokens = 6` rescues things, just barely. So tokens 6 is OK *if* material is perfectly balanced. This narrows my hypothesis: it's not "tokens alone" but "tokens given imperfect material" — and material imperfection makes the token effect worse. I'd hand off to Dr. M here.

**Iter 4 — `(0, 10, 0, 0)`: FAIL, err 0.132.** Pushing tokens further at `max_diff = 0` breaks. So even with parity, beyond 6 tokens the model can't track. This is consistent with my "tokens drive complexity" prior.

**Iter 5 — `(1, 4, 1, 1)`: PASS, err 0.059.** I went back to `max_tokens = 4` and tried relaxing the race-exclusion to `(1, 1)` — allowing states where both sides have exactly 1 immediate-win cell. **Coverage jumped to 46.9% with even better max abs err than B0**. Race conditions at low token counts are not actually a problem. This is news.

**Iter 6 — `(1, 5, 1, 1)`: FAIL, err 0.165.** Trying to keep the race relaxation while pushing tokens to 5 broke it. So race relaxation is fragile to token count.

**Iter 7 — `(0, 5, 1, 0)`: PASS, err 0.037.** At material parity with token 5 and asymmetric race relaxation (allow `iw_tm` up to 1), the boundary holds tightly — err 0.037 is the best I've seen. Coverage 27.9%, similar to Dr. M's `(0, 6, 0, 0)`. So we have multiple viable boundaries with different coverage profiles.

## Final boundary (Dr. B's proposal)

`B_B = (max_diff = 1, max_tokens = 4, race_a = 1, race_b = 1)`

- Coverage: **0.469**
- Max abs error: **0.059**

I recommend this over `(0, 6, 0, 0)` because at low token counts (≤ 4) we don't actually need material parity — even ±1 with both-side races allowed works. Coverage gain is ~17 percentage points.

## Closing reflection (Dr. B)

I came in believing tokens were the dominant axis. The data partly supports me — token expansion does break boundaries quickly — but I had to admit (iter 3, iter 7) that material parity also matters, and that race-condition relaxation is *cheap* at low token counts. My core insight: **tokens and material interact**, but at low tokens the race exclusion is over-conservative.

I think the consensus discussion should explore the (`max_diff`, `max_tokens`) joint surface — there's likely a Pareto frontier of (coverage, error) that none of us alone can fully map.
