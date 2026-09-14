# Flies and Goos: exhaustive analysis

Three exhaustive analyses over the full codon space (36³ = 46,656 codons):
which codons tie on every contest, which codon is strongest against a random
opponent, and which codon best covers the strongest one's losses.

Every result is exact — nothing here is sampled or estimated. The numbers come
from `flies_and_goos.engine`, a vectorized evaluator validated against
`flies_and_goos.game`, the readable reference implementation that passes the 10
supplied test cases. See [Correctness](#correctness) for how the two are
cross-checked.

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/ties.py       # ~0.5s
UV_CACHE_DIR=.uv-cache uv run python analysis/winrates.py   # ~105s, writes beat_matrix.npy
UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py    # ~30s, needs the above
```

> **Rules version.** These results reflect `rules.docx` as of commit 8b58140,
> which redefined contest #6 as *T-junctions plus loops tangent to the floor*
> and made tiebreaks directional (Foe steps forward, Friend steps back). Both
> changes move the numbers substantially; see
> [Effect of the rules update](#effect-of-the-rules-update).

## Findings at a glance

| | |
|---|---|
| **Codons tying all 10 contests** | They exist — 319,812 ordered non-identical pairs, and every one of them genuinely trades positions. |
| **Best codon** | `BFN`, winning 69.25% against a uniform opponent. Worst is `PPD` at 36.80%. |
| **Only the multiset matters** | P(win) is *exactly* invariant under permuting a codon, so the game has **8,436 distinct strategies, not 46,656**. |
| **No ranking exists** | The beat relation is full of cycles: in 44.9% of matchups the lower-rated codon wins. `BFN` loses to `AFF`, the second-weakest multiset. |
| **Best partner for `BFN`** | `BPN` defeats 85.8% of the codons that beat `BFN`. Only 4.37% of codons beat both. |
| **…but arrangement matters there** | Unlike P(win), partner coverage is *not* permutation-invariant: `BPN` covers 12,302 where `PNB` covers 10,300. |

---

## 1. Non-identical codons that tie all 10 contests

Earlier versions of `rules.docx` asked whether non-identical codons could tie
every contest. **They can: 319,812 ordered non-identical pairs** — about 0.015%
of all matchups.

### The reduction

A brute-force sweep is 46,656² ≈ 2.2 billion matchups. It collapses to under a
second via two observations:

1. **Relationship status cannot affect whether a matchup is a draw.** Friend
   awards a position to the *lower* score and Foe to the *higher*, so in both
   cases the contest is tied exactly when `#{a < b} == #{a > b}`.
2. **Nor can the tiebreak direction.** Friend steps back through the contests
   and Foe forward, but stepping ±1 mod 10 visits all ten either way, so "ties
   every contest" is order-independent.

Writing `v(x,y)` for the 10-vector of `sign(score_c(x) − score_c(y))`, codons A
and B tie all 10 contests iff

```
v(a₁,b₁) + v(a₂,b₂) + v(a₃,b₃) = 0        (in Z¹⁰)
```

The 1,296 ordered character pairs carry only 893 distinct signature vectors, so
counting triples that sum to zero is 893² hash operations.

### Results

| | ordered pairs |
|---|---:|
| Tie all 10 contests | 366,468 |
| — of which identical (A = B) | 46,656 |
| **Non-identical ties** | **319,812** |

Every non-identical tie is now a genuine one. Before the rules update, `6`/`9`
and `O`/`S` scored identically in all 10 contests, so `6AB` vs `9AB` tied by
construction — a degenerate 17,344 of the total. Contest #6's floor-loop term
separates both pairs (`6` and `O` gained a point, `9` and `S` did not), so
**no two characters are functionally identical any more** and every tie comes
from positions actually cancelling out.

Verified witnesses (each confirmed a draw by `game.face_off`, all 10 rounds
played):

```
016 vs 180      03G vs 3G0      03Y vs 3Y0
061 vs 108      03K vs 3K0      0CZ vs 3K0
01L vs 1Q1      03V vs 3V0      0G3 vs 30G
0L1 vs 11Q      03X vs 3X0      0K3 vs 30K
```

Note `0G3 vs 30G` and `0K3 vs 30K`: a codon can still draw with a *permutation*
of itself. Both players start at the same contest, since stability is unchanged
by permutation, and the position-by-position wins happen to cancel in all ten.

Ties are rare per codon: a codon draws with 7.9 opponents on average out of
46,656, and every codon draws with at least itself. The most draw-prone is
`99E` with 53.

### Near-identical characters

No character pair is functionally identical, but six pairs differ in exactly one
contest — the closest the alphabet comes to redundancy:

| pair | differs only in |
|---|---|
| `6`/`9`, `O`/`S` | contest #6 (T-junctions + floor loops) |
| `8`/`9`, `C`/`S` | contest #8 (side bumps) |
| `K`/`X` | contest #1 (orthogonal lines) |
| `K`/`Y` | contest #0 (stability) |

Contest #6 is thus load-bearing: without its new loop term, two of these six
pairs collapse into genuine duplicates.

---

## 2. Best codon against a uniformly random opponent

Full 46,656 × 46,656 sweep, 2.18 billion matchups, ranked by P(win).

### The winner: `BFN` (and its permutations), P(win) = 0.6925

Top 10 distinct multisets:

| codon | type | stab | P(win) | draws |
|---|---|---:|---:|---:|
| **BFN** | Fly | 5 | **0.6925** | 6 |
| NBT | Fly | 5 | 0.6844 | 11 |
| 3NN | Fly | 5 | 0.6789 | 7 |
| NNS | Fly | 5 | 0.6776 | 7 |
| NNO | Fly | 5 | 0.6710 | 3 |
| NPE | Fly | 5 | 0.6694 | 4 |
| CNN | Fly | 5 | 0.6674 | 7 |
| FBH | Fly | 5 | 0.6667 | 6 |
| 3HN | Fly | 5 | 0.6666 | 10 |
| FND | Fly | 5 | 0.6665 | 4 |

Weakest multisets:

| codon | type | stab | P(win) |
|---|---|---:|---:|
| PPD | Goo | 3 | 0.3680 |
| AFF | Fly | 2 | 0.3693 |
| CGP | Goo | 3 | 0.3727 |
| TFA | Fly | 2 | 0.3760 |

The full ranking of all 46,656 codons is in `winrates.tsv` (gitignored;
regenerate with the command above).

### Only the multiset of characters matters

**P(win) is exactly invariant under permuting a codon's characters.** All 46,656
codons fall into 8,436 multiset classes, and within every class the win, loss
and draw counts are identical — verified, not sampled.

The reason: contests compare position-by-position, so playing a permuted codon
`A∘π` against opponent `B` is the same as playing `A` against `B∘π⁻¹`. Stability
and Fly/Goo type are both permutation-invariant, so the relationship, the
opening contest *and the tiebreak direction* are unchanged, and permuting a
uniformly random opponent leaves it uniformly random. `BFN`, `BNF`, `FBN`,
`FNB`, `NBF`, `NFB` are one strategy, not six.

**So there are only 8,436 genuinely distinct choices in this game**, not 46,656.

### Flies have a real edge

| type | n | mean P(win) | best | worst |
|---|---:|---:|---:|---:|
| Fly | 23,328 | 0.5115 | 0.6925 (`BFN`) | 0.3693 |
| Goo | 23,328 | 0.4883 | 0.6123 (`88M`) | 0.3680 |

Being a Fly is worth about 2.3 percentage points on average, and the top 10
multisets are all Flies. This is not a counting artifact: the type split is
exactly even (23,328 each), since a codon is a Fly when at least 2 of its 3
characters are straight and the alphabet is 18/18 straight/curved.

### Stability 5 dominates the top, but not the bottom

| stability | n | mean P(win) | best |
|---:|---:|---:|---|
| 0 | 512 | 0.4931 | 0.5551 (`7PQ`) |
| 1 | 2,112 | 0.5074 | 0.6158 (`044`) |
| 2 | 4,632 | 0.4970 | 0.6051 (`88Y`) |
| 3 | 7,619 | 0.4742 | 0.5770 (`44B`) |
| 4 | 9,435 | 0.5058 | 0.6246 (`4GN`) |
| **5** | **9,033** | **0.5169** | **0.6925 (`BFN`)** |
| 6 | 7,017 | 0.5111 | 0.5756 (`0EM`) |
| 7 | 4,056 | 0.4889 | 0.5449 (`26B`) |
| 8 | 1,728 | 0.4784 | 0.5553 (`GII`) |
| 9 | 512 | 0.4844 | 0.5363 (`1BZ`) |

Stability 5 has both the highest mean and the best codon, and all ten top
multisets sit there. The weakest codons, by contrast, are at stability 2–3. The
mechanism is not established here; stability enters twice, as contest #0's score
and as the selector for the opening contest.

### There is no ranking — the beat relation is full of cycles

P(win) orders codons by how they do against the *field*, but it is close to
useless for predicting a specific matchup. Sampling 200,000 random pairs,
**the lower-rated codon wins 44.9% of the time** — barely distinguishable from a
coin flip.

Concretely, `HB1` beats `VTS` beats `GKB` beats `HB1`, with P(win) values of
0.5128, 0.4985 and 0.5095 respectively. And `BFN`, the strongest codon in the
game, loses head-to-head to `AFF`, whose 0.3693 is the second-weakest of all
8,436 multisets.

So a player who knows their opponent's tendencies should not just play `BFN`.

### Distribution

P(win) spans 0.3680 to 0.6925 with median 0.4983; 48.3% of codons are above
0.5. Codon choice is worth real money — the best choice wins roughly 69% against
a random opponent versus the worst choice's 37% — but with cycles this dense,
Phil can still get lucky.

---

## 3. The best partner for `BFN`

`BFN` wins most often, but 14,343 codons (30.74%) still beat it. Which single
codon cleans up the most of that set?

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py --codon BFN
```

### The answer: `BPN`, which defeats 12,302 of BFN's 14,343 conquerors (85.8%)

| partner | covers | frac | beat both | partner's own P(win) |
|---|---:|---:|---:|---:|
| **BPN** | **12,302** | **0.8577** | **2,041** | 0.4437 |
| DPM | 12,280 | 0.8562 | 2,063 | 0.4182 |
| BPM | 12,226 | 0.8524 | 2,115 | 0.4546 |
| BFR | 12,184 | 0.8495 | 2,159 | 0.4240 |
| BFG | 12,170 | 0.8485 | 2,169 | 0.4217 |
| RUM | 11,920 | 0.8311 | 2,419 | 0.4934 |
| E5U | 11,877 | 0.8281 | 2,465 | 0.5097 |

`BPN` is simultaneously the *global* minimiser of "codons that beat both", at
2,041. The two objectives pick the same codon, as expected — coverage and shared
losses sum to 14,343 minus the handful of draws.

### As a pair, `{BFN, BPN}` is very hard to sweep

| | beaten by |
|---|---:|
| `BFN` alone | 30.74% of codons |
| **`{BFN, BPN}` both** | **4.37%** of codons |

Only 2,041 of 46,656 codons beat both. (This is the two-codon duo question
restricted to `BFN` as one member, so it upper-bounds the best duo overall
without attaining it.)

### If you want both halves playable: `E5U`

Most strong partners are mediocre codons in their own right. `E5U` is the best
partner that is itself above average — P(win) 0.5097, covering 82.8% of BFN's
conquerors. `{BFN, E5U}` is beaten by 5.28% of the field, against 4.37% for
`{BFN, BPN}`, which buys a genuinely competitive second codon.

### Partner quality is unrelated to codon quality

The correlation between partner coverage and P(win) across all 46,656 candidates
is −0.0442 — essentially zero. Weak codons cluster near the top of the partner
ranking, but being weak predicts almost nothing about being a useful partner.
The *worst* partners are strong codons (`BTN`, 643; `BFM`, 918 — both above 0.65
P(win)), which is the same phenomenon in reverse: codons resembling `BFN` lose to
the same opponents it does.

### Arrangement matters here, unlike P(win)

Part 2 showed P(win) is exactly permutation-invariant. **Coverage is not:**

| arrangement | covers |
|---|---:|
| BPN | 12,302 |
| PBN | 11,491 |
| NBP | 11,150 |
| NPB | 11,092 |
| BNP | 10,925 |
| PNB | 10,300 |

A 2,002-codon spread across one multiset. The invariance proof in part 2 relied
on the opponent being *uniformly* random, so that permuting it changes nothing.
`L(BFN)` is a fixed, arbitrary set with no such symmetry, so the specific
arrangement matters — picking `PNB` instead of `BPN` throws away a sixth of the
coverage. Choosing a partner is a genuinely 46,656-way choice, not an 8,436-way
one.

---

## Effect of the rules update

Commit 8b58140 changed two things, and both moved the results:

**Contest #6** became *T-junctions plus loops tangent to the floor*, adding a
point to exactly the characters whose enclosed space rests on the floor:
`0` 2→3, `6` 1→2, `8` 0→1, `B` 1→2, `D` 0→1, `O` 0→1. Notably `9`, `P`, `Q`,
`R` and `A` do not gain, since their loops sit clear of the floor.

**Tiebreaks became directional** — Foe pairs increment the contest as before,
Friend pairs now decrement (#0 wrapping to #9).

| | before | after |
|---|---:|---:|
| Ordered pairs tying all 10 | 423,580 | 366,468 |
| Non-identical ties | 376,924 | 319,812 |
| — of which degenerate substitutions | 17,344 | **0** |
| Interchangeable character pairs | 2 (`6`/`9`, `O`/`S`) | **0** |
| Best codon | `ENP`, 0.6294 | `BFN`, 0.6925 |
| Worst codon | `PGE`, 0.3619 | `PPD`, 0.3680 |
| Codons above P(win) 0.5 | 51.9% | 48.3% |

The most interesting consequence is the elimination of interchangeable
characters. All 36 characters are now functionally distinct, so the alphabet
carries no redundancy and every tie is earned.

Two findings survived the change unchanged: exact permutation-invariance of
P(win) (the argument never depended on contest order), and the Fly advantage of
roughly 2.3 points. One did not: under the old rules the best codon lost to the
*worst* and the best partner for `ENP` was the single weakest codon in the game.
Both were coincidences of that rule set — `BFN` beats `PPD`, and `BPN` is a
mid-tier codon. The general non-transitivity is real and survives; the tidy
extremal version of it did not.

## Correctness

Everything above rests on `flies_and_goos.engine`, a vectorized reimplementation
of the rules. It is checked against the readable reference implementation
(`flies_and_goos.game`, which passes the 10 supplied test cases) by:

- agreement on all 10 supplied cases and 2,000 pseudo-random matchups;
- antisymmetry, `outcome(A,B) == −outcome(B,A)`;
- per-codon stability and Fly/Goo type matching `game.Codon`;
- wins summed over all codons equalling losses summed over all codons;
- **an independent cross-check**: the full sweep counts 366,468 drawn matchups,
  exactly the number the signature-sum method of part 1 derives without playing
  a single game. Two unrelated algorithms agreeing on that figure is the
  strongest evidence here that both the new contest #6 table and the directional
  tiebreak are implemented consistently.

The 10 supplied test cases all keep the same winners under the updated rules, so
they cannot on their own detect a tiebreak-direction bug. `tests/test_game.py`
therefore adds cases chosen to fail under the old rules: `1A7` vs `T3P` (a
Friend pair that ties #6 and is decided on #5, where incrementing would give the
win to `T3P`), and five matchups decided on contest #6 whose winners flip if the
floor-loop term is dropped.

Part 3 reads the bit-packed beat-matrix, where a transposed read would give a
plausible but wrong answer, so `analysis/partner.py` re-derives the top 10
partners by replaying those matchups through the engine and asserts agreement
before printing anything.

## Not done

The **unrestricted** two-codon search — the best duo over all ~1.09 billion
pairs, rather than pairs containing `BFN` — is still open. Part 3 gives the best
`BFN` duo at 4.37%, which upper-bounds the global optimum but does not attain
it. `flies_and_goos.duo.coverage_counts` is written generically over a target
set, so it is the building block for that sweep.
