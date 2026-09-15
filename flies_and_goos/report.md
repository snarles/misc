# Flies and Goos: exhaustive analysis

Five analyses over the full codon space (36³ = 46,656 codons): which codons tie
on every contest, which codon is strongest against a random opponent, which
codon best covers the strongest one's losses, which *triples* of codons nothing
can sweep, and the strongest *pair* of codons sharing no character.

Parts 1-3 and 5 are exact — nothing in them is sampled or estimated. Part 4 is a
sampled heuristic search over 500 climbs and says so throughout; its existence claims are
proven, its rarity claims are not. The numbers come
from `flies_and_goos.engine`, a vectorized evaluator validated against
`flies_and_goos.game`, the readable reference implementation that passes the 10
supplied test cases. See [Correctness](#correctness) for how the two are
cross-checked.

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/ties.py       # ~0.5s
UV_CACHE_DIR=.uv-cache uv run python analysis/winrates.py   # ~6min, writes beat_matrix.npy
UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py    # ~30s, needs the above
UV_CACHE_DIR=.uv-cache uv run python analysis/fortress.py   # ~22min, needs the above
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
| **Perfect 3-fortresses exist** | Eleven of them found — three codons sharing no characters which *nothing* beats, e.g. `005` `CX4` `JNJ` and `555` `3MV` `JJJ`. |
| **Fortresses must mix Fly and Goo** | All 500 searched fortresses are mixed-type; the best same-type triple found is 10x worse. |
| **Specialisation needs three** | `JJJ` anchors every perfect triple but is *harmful* in a pair (4,003 totalizers): with two members nothing covers a keystone's blind side. |
| **Best pair overall** | `BDW` `BEW`, beaten by only 390 codons (0.84%) — `{BFN, BPN}` at 2,041 is 5.2x off. Best pair sharing no character: `035` `64W` at 1,119. |
| **A pair has a floor; a triple does not** | No legal pair concedes fewer than 1,119 totalizers, yet eleven triples concede none. |
| **`JJJ` is a keystone** | All 11 perfect fortresses contain a J-repeating codon, and such fortresses have a median 9 totalizers against 135 — yet `JJJ` alone has a below-average P(win) of 0.4671. |

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

`BPN` is simultaneously the minimiser of "codons that beat both" *among BFN's
partners*, at 2,041 — not the global minimiser over all pairs, which part 5
shows is far lower. The two objectives pick the same codon, as expected — coverage and shared
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

### The unrestricted best duo: `BDW` `BEW` at 390

The question this part does not answer — the best duo over all ~1.09 billion
pairs rather than pairs containing `BFN` — is settled in
[part 5](#5-2-fortresses-the-strongest-legal-pair), which sweeps every pair
exhaustively. The answer is **`BDW` `BEW`, beaten by only 390 codons (0.84%)**.

So `{BFN, BPN}` at 2,041 is **5.2x away from the global optimum**. Building a duo
around the strongest single codon is a poor strategy: `BFN` is the best codon in
the game on its own and appears nowhere in the top unrestricted duos, which are
all `B?W` shapes. Individual strength and partnership value are close to
unrelated, which the "Partner quality is unrelated to codon quality" result above
already hinted at from the other direction.

`BDW` and `BEW` share `B` and `W`, so this duo is legal here but not a
2-fortress; part 5 gives the best character-disjoint pair separately.

---

## 4. 3-fortresses: triples nothing can sweep

A **3-fortress** is three codons no two of which share a character (repeats
*inside* one codon are allowed). A **totalizer** is a codon that beats all
three. A fortress is strong when it has few totalizers — pick one of its three
codons and no single opponent is favoured against your whole hand.

Unlike parts 1-3 this part is **not exhaustive**. There are ~10^13 valid
triples; this samples 500 at random and hill-climbs each by replacing one codon
at a time with whichever legal codon most reduces the totalizer count, stopping
at a local optimum. All 500 converged rather than hitting the step cap.
Existence claims below are proven; absence and rarity claims are not.

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/fortress.py --seeds 100
UV_CACHE_DIR=.uv-cache uv run python analysis/polarizer.py --csv fortresses.csv
UV_CACHE_DIR=.uv-cache uv run python analysis/pair.py --exhaustive  # ~15min
```

### Perfect fortresses exist, and there are at least eleven

Sixteen of the 500 climbs reached **zero totalizers** — not one of the 46,656
codons defeats all three members. Those sixteen are eleven distinct fortresses,
counting up to the permutation symmetry described below.

| fortress | types | stability |
|---|---|---|
| **`005` `CX4` `JNJ`** | Goo/Fly/Goo | 3/3/4 |
| **`CNV` `P5W` `JJJ`** | Fly/Goo/Goo | 3/3/3 |
| **`055` `JJJ` `3MV`** | Goo/Goo/Fly | 3/3/3 |
| **`JXJ` `KVK` `BBR`** | Goo/Fly/Goo | 4/4/8 |
| **`055` `CNV` `JJJ`** | Goo/Fly/Goo | 3/3/3 |
| **`055` `6MV` `JJJ`** | Goo/Fly/Goo | 3/3/3 |
| **`WP5` `CMY` `JJJ`** | Goo/Fly/Goo | 3/3/3 |
| **`555` `3MV` `JJJ`** | Goo/Fly/Goo | 3/3/3 |
| **`055` `JJJ` `SNV`** | Goo/Goo/Fly | 3/3/3 |
| **`5BB` `JJJ` `VAU`** | Goo/Goo/Fly | 7/3/3 |
| **`055` `JJJ` `OMV`** | Goo/Goo/Fly | 3/3/3 |

So "very few totalizers" bottoms out at none. Against any opponent, at least one
of the three codons is not beaten — the opponent either loses to it or draws.

Each was confirmed against `game.face_off` rather than trusted from the matrix,
where a transposed or mis-masked read would manufacture exactly this result.
For each fortress, every codon beating exactly two of its members — between
22,812 and 26,970 of them — was replayed through the reference rules, and none
beats the third.

### They are not isolated points

The eight are not scattered solutions. `055` `JJJ` `3MV` has two immediate
neighbours in the set, `055` `6MV` `JJJ` and `555` `3MV` `JJJ`, each differing
in a single character; since no permutation alters characters, those are
genuinely distinct fortresses rather than one orbit. Others recombine members:
`055` `CNV` `JJJ` takes `055` from one perfect fortress and `CNV` from another,
and `WP5` `CMY` `JJJ` reuses a permutation of the `P5W` in `CNV` `P5W` `JJJ`.
Whatever makes a fortress perfect survives local edits, which suggests a
structural cause rather than a numerical coincidence.

### Every perfect fortress is built on a J-repeating codon

All eleven contain a codon whose repeated character is `J` — nine contain `JJJ`
outright, the others `JNJ` and `JXJ`. This is the sharpest pattern in the data,
and it holds across the whole sample rather than only at the extreme:

| | fortresses | median totalizers |
|---|---:|---:|
| contains a J-repeating codon | 52 | **9** |
| does not | 448 | 135 |

A 15x gap, and **all 16 zero-totalizer climbs** fall in the first row. `J` is
also the most over-represented character in the strongest third of results (84
occurrences against 29 in the weakest third).

The last 200 climbs were run after this section was first written, so they are a
genuine held-out test of the claim rather than the data it was fitted to. All
three perfect fortresses discovered in them contain a J-repeating codon.

The striking part is that `JJJ` is a **bad codon on its own**: it wins 21,792
matchups and loses 24,863, for P(win) = 0.4671, well below average.

The mechanism is the same Friend/Foe inversion as above. `J` is one of the most
extremal characters in the game, scoring 0 in six of the ten contests and never
reaching a top set — its scores are `[1,1,0,1,0,0,0,0,0,1]`. Being uniformly
*low* is worthless in a Foe matchup, where the higher score takes the position,
and close to decisive in a Friend matchup, where the lower one does. `JJJ` is a
Goo, so it plays Friend against every Fly and Foe against every Goo. The result
is a codon that splits the opponent space almost perfectly in half:

| | `JJJ` beats |
|---|---:|
| the 23,328 Flies | **20,786 (89.1%)** |
| the 23,328 Goos | 1,006 (4.3%) |

That is what makes it a keystone rather than a good codon. One member disposes
of nearly the entire Fly half of the opponent space, leaving its two partners to
cover only Goos — and since a fortress must be mixed, at least one partner is a
Fly, which is Foe to Goo attackers and so pulls in the opposite direction. Its
mediocre P(win) is the *price* of that specialisation, not evidence against it.

Ten of the eleven also contain a `5`; `JXJ` `KVK` `BBR` is the exception, and
it is the only perfect fortress that is all letters and not low-stability.

### The reachable range

Random valid triples start around 5,500 totalizers and greedy takes them to a
median of **121** — a median 45x reduction — in 2 to 12 swaps.

| percentile | 0 | 10 | 25 | 50 | 75 | 90 | 100 |
|---|---:|---:|---:|---:|---:|---:|---:|
| final totalizers | 0 | 10 | 60 | 121 | 228 | 354 | 815 |

110 of 500 finished at 50 or fewer, 220 at 100 or fewer. The worst figure is the
least stable one here: the maximum rose from 586 at 300 climbs to 815 at 500, so
treat it as the worst seen rather than any kind of bound.

### Mixing Fly and Goo is mandatory in every sample taken

**All 500 final fortresses are mixed** — 334 Fly/Fly/Goo and 166 Fly/Goo/Goo.
The random seeds included 41 all-Fly and 58 all-Goo triples, and the climb moved
every one of those 99 out of homogeneity.

This is not an artefact of where the search started. Sampling 4,000 random
same-type triples directly, the best all-Fly triple found has 1,672 totalizers
and the best all-Goo 1,177 — an order of magnitude worse than the median mixed
result.

The mechanism is the Friend/Foe rule. An attacking Fly is a Foe to Fly members
(higher score takes the position) but a Friend to Goo members (lower score takes
it). A mixed fortress therefore demands that a totalizer be simultaneously
high-scoring and low-scoring on the contests it lands in. A homogeneous fortress
imposes one consistent direction, and a single extremal codon can sweep it.

### Strong fortresses are letter-heavy and repeat-heavy

Character slots in the strongest third are **21% digits**, against 31% in the
weakest third. Repeated-character codons are **55%** of the strongest third and
**30%** of the weakest — a pattern that was soft at 100 climbs and is stable
from 300 to 500.

| | strongest third | weakest third |
|---|---:|---:|
| `J` | 84 | 29 |
| `W` | 84 | 57 |
| `L` | 48 | 26 |
| `7` | 6 | 43 |
| `1` | 8 | 44 |
| `2` | 15 | 49 |

### Character frequency at the strong end

Narrowing to the 71 climbs that finished at 30 totalizers or fewer, and
deduplicating by the permutation symmetry, leaves **42 distinct fortresses** —
126 codons, 378 character slots. Counts below are against a baseline of the
characters in all 500 final fortresses rather than a uniform 1/36, so the lift
isolates what makes a fortress *strong* rather than what makes it a fortress at
all.

| char | count | share | baseline | lift |
|---|---:|---:|---:|---:|
| **`J`** | **62** | **16.40%** | 5.20% | **3.15x** |
| `W` | 31 | 8.20% | 6.64% | 1.23x |
| `5` | 26 | 6.88% | 3.93% | 1.75x |
| `X` | 21 | 5.56% | 3.20% | 1.74x |
| `L` | 18 | 4.76% | 3.16% | 1.51x |
| `0` | 17 | 4.50% | 3.80% | 1.18x |
| `M` | 17 | 4.50% | 4.73% | 0.95x |
| `U` | 16 | 4.23% | 3.62% | 1.17x |
| `K` | 15 | 3.97% | 2.27% | 1.75x |
| `V` | 15 | 3.97% | 1.76% | **2.26x** |
| `B` | 15 | 3.97% | 4.40% | 0.90x |
| `4` | 14 | 3.70% | 4.33% | 0.85x |

The most depleted are `8` (0.10x), `1` (0.16x), `I` (0.19x), `2` (0.23x),
`H` (0.31x) and `7` (0.37x).

`J` dominates by a wide margin — 3.15x the fortress baseline, 5.9x uniform, and
more than double the share of the next character. `V` is the only other strong
outlier at 2.26x. But **every one of the 36 characters appears at least once**,
so nothing is disqualified from a strong fortress; the effect is distributional,
not a hard constraint.

The letter/digit skew is milder here than the tercile split above suggests:
79.4% letters against a 74.9% baseline. Digits are not excluded so much as
concentrated — `5` is the third-commonest character of all, with `0` and `4` also
near the top, while `1`, `2`, `7` and `8` are close to absent. Curved and
straight characters are split 51.1% / 48.9%, and the codons divide 62 Fly to 64
Goo, so neither glyph class nor codon type discriminates at this threshold. It is
the identity of the character that matters, not its shape family.

One caveat on `J`'s 16.40%: it comes from repetition within codons more than from
breadth. `J` occurs somewhere in 29 of the 42 fortresses (69%), but only 20 of 42
(48%) contain a J-*repeating* codon. The 11-of-11 J-repeat pattern belongs to the
**perfect** fortresses specifically; a 30-totalizer threshold is loose enough to
admit many strong fortresses built on other characters entirely.

### Strong fortresses divide labour between a polarizer and two mop-ups

The `JJJ` mechanism above is not a quirk of one codon. Define a codon's
**polarity** as `wF - wG`, the difference between the fraction of the 23,328
Flies and the fraction of the 23,328 Goos it defeats; call the member with the
largest `|polarity|` the **polarizer** and the other two the **mop-ups**. The
structure sharpens monotonically with fortress strength:

| band | n | mean \|polarity\| | polarizer's cover of its type | mop-ups' cover of the other type |
|---|---:|---:|---:|---:|
| **perfect (0)** | 11 | **0.833** | **0.888** | **0.994** |
| ≤ 30 | 42 | 0.699 | 0.847 | 0.965 |
| 31-120 | 124 | 0.596 | 0.812 | 0.956 |
| > 120 | 226 | 0.469 | 0.758 | 0.924 |
| random seed triples | 200 | 0.412 | 0.703 | 0.757 |

Against a mean `|polarity|` of 0.246 across all 46,656 codons, every band is
polarized above chance and the perfect fortresses are extreme: their polarizer
defeats 88.8% of one whole type, and their two mop-ups between them defeat 99.4%
of the other. `analysis/polarizer.py` produces the table; figures are over the
392 distinct fortresses, deduplicated by the permutation symmetry.

### The route split is what forces type-mixing

Classifying *how* each member covers its type — **Foe-high** when member and
covered type match, so the higher score takes the position, **Friend-low** when
they differ and the lower score does — gives two signatures that are perfect
across all eleven zero-totalizer fortresses:

- the polarizer covers its type **Friend-low in 11 of 11**;
- the two mop-ups are **exactly one Foe-high and one Friend-low in 11 of 11**.

That second fact is the type-mixing result derived from the other direction. A
fortress needs both routes represented, and having both requires members of both
types. Weaker bands break the pattern freely: at > 120 totalizers, 38 fortresses
pair two Foe-high mop-ups and 19 pair two Friend-low.

There is also a clean asymmetry. **Every perfect fortress polarizes against
Flies, never Goos** (11 of 11; 34 of 42 in the ≤ 30 band), while the weak bands
are an even split. No Goo-side mirror of `JJJ` turned up anywhere in the search:
Goo-polarized fortresses stall well short of perfection.

### Where the hypothesis is only half right

The natural strong reading — that the mop-ups handle the type the polarizer
leaves open, so the surviving totalizers should sit on the mop-ups' side — does
**not** hold. Splitting each fortress's totalizers by opponent type gives a
roughly even division in every band:

| band | totalizers of the polarizer's type | of the other type |
|---|---:|---:|
| ≤ 30 | 246 | 185 |
| 31-120 | 5,174 | 4,302 |
| > 120 | 28,005 | 30,716 |

So the division of labour is real in *coverage* but not in *residue*. The
polarizer takes the bulk of one type and the mop-ups take almost all of the
other, yet the 11% of its own type the polarizer misses is large enough that the
mop-ups must cover that too — and what survives is split evenly rather than
concentrated. A fortress is not two independent halves.

One near-perfect fortress takes a different route entirely: `06Q` `4AV` `WWW` at
1 totalizer is polarized by `WWW` **Foe-high**, with `|polarity|` only 0.403 —
below the average for the > 120 band. Polarization is a strong tendency at the
top, not a requirement, and the next section takes that apart.

These results are correlational. They establish that strong fortresses have this
structure, not that the structure is what makes them strong; no fortress was
constructed from the theory to test that.

### Two routes to a strong fortress: keystone and committee

Polarization is strongly predictive but far from deterministic — the rank
correlation between a fortress's `|polarity|` and its totalizer count is −0.449,
and the residuals are not noise. They mark a second architecture.

Define a fortress's **teamwork premium** as how much the three members together
cover beyond what its best single member covers, averaged over the two types.
Premium and polarity are near-perfect substitutes, at a rank correlation of
**−0.902**: a fortress either has one member that does the work, or three that
divide it.

| | n | median totalizers | best | mean premium |
|---|---:|---:|---:|---:|
| **keystone** (`\|polarity\|` ≥ 0.5) | 223 | 99 | **0** | +0.199 |
| **committee** (`\|polarity\|` < 0.5) | 169 | 201 | 1 | +0.315 |

The clearest committee is `TTO` `544` `7Q3` at 28 totalizers, the largest
residual in the sample. Its *most* polarized member sits at 0.236 — below the
0.246 mean for an average codon — so it has no polarizer at all:

| codon | type | beats Flies | beats Goos | \|polarity\| |
|---|---|---:|---:|---:|
| `TTO` | Fly | 0.367 | 0.603 | 0.236 |
| `544` | Fly | 0.653 | 0.517 | 0.136 |
| `7Q3` | Goo | 0.603 | 0.456 | 0.147 |

No member beats more than 65% of either type, yet between them they beat 100.0%
of Flies and 99.9% of Goos, because their misses barely overlap. Its premium is
+0.347 on Flies and +0.396 on Goos, against +0.109 and +0.151 for the keystone
fortress `055` `JJJ` `3MV`. Three mediocre generalists, carefully
non-overlapping, versus one specialist and two passengers.

### The committee route has a ceiling

Committees reach 25-28 totalizers and stop. Every perfect fortress is
keystone-built, and the threshold is sharp:

| `\|polarity\|` | n | best totalizer count | zeros |
|---|---:|---:|---:|
| 0.0 - 0.3 | 51 | 28 | 0 |
| 0.3 - 0.4 | 56 | 25 | 0 |
| 0.4 - 0.5 | 62 | 1 | 0 |
| 0.5 - 0.6 | 74 | 3 | 0 |
| 0.6 - 0.7 | 65 | 2 | 0 |
| **0.7 - 1.0** | 84 | **0** | **11** |

**No fortress below 0.7 polarity reached zero**, and all eleven that did sit
above it. Coordination gets you to within a couple of totalizers of perfection —
`06Q` `4AV` `WWW` manages 1 at a polarity of 0.403 — but closing the last gap
appears to need a specialist that erases most of one type single-handedly.

Polarization is not sufficient either. `JYY` `70Q` `MWB` has `|polarity|` 0.844,
close to `JJJ`'s 0.848, and still concedes 317 totalizers: a keystone whose
partners fail to cover the complement buys nothing. Both halves of the structure
have to hold.

### The landscape is rugged, and these are local optima

500 climbs produced **392 distinct local optima**, and the starting score is
essentially uninformative about the finish: Spearman −0.034 between initial and
final totalizers. Consecutive batches of ten swung between medians of 39 and
256. Greedy converges fast and to wildly different places.

Some optima are nevertheless found repeatedly: `DHU` `KKK` `WEW` at 10
totalizers was reached six separate times from unrelated seeds, and four of the
eleven perfect fortresses were hit more than once. The landscape has many basins,
but they are not uniformly small — see below.

**No claim of optimality is made here.** The eleven perfect fortresses are
proven to exist, but whether they are rare or abundant is not settled, and
nothing here bounds how far a local optimum sits from the global best. The
16-in-500 hit rate is a property of this search procedure, not an estimate of
their density. New perfect fortresses were still appearing in the final batch of
500, so the count is a floor and nothing suggests it has saturated.

### The largest basins of attraction

Counting how many of the 500 climbs converge to each optimum estimates the
relative size of its basin — how much of the seed space drains into it. 392
distinct optima came out of 500 climbs, but they are far from equally reachable:

| optima | reached | climbs |
|---:|---:|---:|
| 3 | 6 times each | 18 |
| 8 | 4 times each | 32 |
| 15 | 3 times each | 45 |
| 39 | twice each | 78 |
| 327 | once | 327 |

**65 optima account for 173 of the 500 climbs (35%)**, while 327 were seen
exactly once. The most-reached:

| hits | totalizers | fortress |
|---:|---:|---|
| 6 | 79 | `9DN` `GIL` `WRM` |
| 6 | 10 | `DHU` `KKK` `WEW` |
| 6 | 60 | `8JN` `RBB` `WHP` |
| 4 | 1 | `9KU` `QWW` `YJL` |
| 4 | 2 | `ANY` `BBR` `JMJ` |
| 4 | 38 | `44X` `55V` `WWR` |
| 4 | 38 | `444` `5BB` `MDW` |
| 4 | 64 | `23B` `LLL` `MWE` |
| 4 | 177 | `0VW` `3NN` `UGM` |
| 4 | 205 | `58D` `KIK` `MFB` |
| 4 | 247 | `123` `AAE` `TIH` |

**Bigger basins tend to hold better fortresses.** Optima reached more than once
have a median of 71 totalizers against 163 for those reached exactly once, and
the rank correlation between hits and totalizer count is −0.205. That is a
mildly encouraging property for this kind of search: the strong solutions are not
needles, and random restarts find them disproportionately often. It is not
universal, though — `9DN` `GIL` `WRM` at 79 and `123` `AAE` `TIH` at 247 both
have large basins and mediocre counts.

Among the perfect fortresses the basins are smaller. `005` `CX4` `JNJ` was
reached three times, `055` `JJJ` `3MV`, `555` `3MV` `JJJ` and `055` `JJJ` `SNV`
twice each, and the remaining seven once. So perfection does not come with an
unusually wide basin — those eleven are found because there are several of them,
not because any one is especially easy to fall into.

Two caveats. A hit count estimates basin volume only under this particular
sampler, which draws the first member uniformly and each subsequent one uniformly
from what stays disjoint — not uniformly over triples. And at 500 samples the
difference between a 6-hit and a 4-hit basin is well inside noise; only the broad
split between repeatedly-found and once-found optima is solid.

### A symmetry worth knowing

Permuting the character positions of *all three* members the same way leaves the
totalizer count unchanged, because the engine satisfies a full conjugation:
`outcome(σ·a, σ·b) == outcome(a, b)`. So `{KKX, WZW, LUR}` and `{KKX, ULR, ZWW}`
are the same fortress wearing different clothes, and distinct results must be
counted up to that 6-fold symmetry or diversity is overstated — five of the
sixteen zero-totalizer climbs were rediscoveries of an already-found fortress
under this symmetry. Note this is *simultaneous* permutation only — permuting one member
alone changes the count, consistent with part 3.

---

## 5. 2-fortresses: the strongest legal pair

A **2-fortress** is two codons sharing no character, scored like a 3-fortress by
its **totalizers** — the codons that beat both. Part 4 raised the question: no
pair inside any 3-fortress came close to the strength of the triples containing
it, and it was unclear whether the disjointness rule or the fortresses themselves
were responsible.

Unlike part 4, **this part is exhaustive**. A pair sweep looks infeasible at
~1.09 billion pairs, but for a fixed first member the totalizer count against
every second member is one pass over the beat-matrix, and the
simultaneous-permutation symmetry means only the 8,436 codons with non-decreasing
characters need to be the first member. Every pair is equivalent to one of those.
The whole sweep runs in about 13 minutes.

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/pair.py --seeds 200 --exhaustive
```

### The answer: `035` `64W`, beaten by 1,119 codons (2.40%)

| pair | totalizers | fraction |
|---|---:|---:|
| **`035` `64W`** | **1,119** | 0.0240 |
| `058` `6W4` | 1,162 | 0.0249 |
| `059` `6W4` | 1,170 | 0.0251 |
| `05S` `6W4` | 1,314 | 0.0282 |
| `48W` `595` | 1,337 | 0.0287 |
| `49W` `585` | 1,348 | 0.0289 |
| `KUU` `YNN` | 1,390 | 0.0298 |

This is a proved optimum, not a search result, and it was re-counted by replaying
all 1,119 matchups through `game.face_off`. The top of the table is narrow:
`6W4` or a permutation of it appears in five of the seven, always paired with a
codon made of `0`, `5` and one other digit.

### Dropping the disjointness rule is worth 3x

The same sweep without the character mask gives **`BDW` `BEW` at 390 (0.84%)**,
also verified against the reference rules. So the no-overlap constraint costs
729 totalizers — the best legal pair concedes nearly three times as many as the
best pair outright. The constraint is expensive, which is worth remembering when
comparing 2- and 3-fortress figures against part 3's duo numbers.

### A pair cannot go below 1,119, and that reframes part 4

Part 4 observed that no pair inside any 3-fortress concedes fewer than 500
totalizers. That is true but not meaningful on its own: **no legal pair anywhere
in the game concedes fewer than 1,119**, so 500 was never reachable.

The real statement is stronger. The best possible pair leaves 1,119 codons
beating it; eleven *triples* leave none at all. Adding a third codon is not an
incremental improvement on a duo — it crosses a floor that no pair can reach,
whatever its members. Against that, the best pair occurring inside a 3-fortress
(1,476) is only 32% off the global pair optimum, while the median fortress's best
internal pair is around 4,500. Fortresses do not generally contain good pairs;
they do not need to.

### Polarization: opposite signs, but moderate ones

Applying part 4's polarity measure (`wF - wG`) to pairs gives a structure that is
universal at the top and then inverts part 4's lesson.

**Every strong pair splits the opponent space between its members.** All eight
best disjoint pairs and all four best unrestricted duos have members of opposite
polarity — one Fly-killer, one Goo-killer. Across the 92 distinct local optima
from the sampled search, 84 (91%) are opposite-sign, against 53% for random
disjoint pairs, so the climb selects hard for it.

| pair | totalizers | pol(a) | pol(b) | covers Flies | covers Goos |
|---|---:|---:|---:|---:|---:|
| **`035` `64W`** | **1,119** | −0.415 | +0.375 | 0.979 | 0.973 |
| `058` `6W4` | 1,162 | −0.549 | +0.375 | 0.965 | 0.984 |
| `KUU` `YNN` | 1,390 | +0.644 | −0.471 | 0.992 | 0.948 |
| `BDW` `BEW` (unrestricted) | 390 | −0.808 | +0.808 | 0.991 | 0.992 |

The route also differs from part 4. Pairs cover almost entirely **Foe-high** on
both sides, where all eleven perfect triples had a **Friend-low** polarizer.
`KUU` `YNN` is the only Friend-low pair in the table.

### More polarization is worse, which is the opposite of part 4

The natural guess — that the disjointness rule costs 3x by blocking extreme
opposite polarization, since `BDW` and `BEW` share `B` and `W` — is **wrong**.
The most oppositely-polarized *disjoint* pair, `JJT` `5BM`, has a polarity spread
of 1.728, effectively equal to the unrestricted maximum of 1.744. Extreme
opposite pairs are available under the constraint. They are simply bad:

| pair | totalizers | spread | covers Flies | covers Goos |
|---|---:|---:|---:|---:|
| `JJT` `5BM` — most polarized disjoint | 3,243 | 1.728 | 0.928 | 0.933 |
| `JJJ` `5BM` — the part 4 keystone, paired | 4,003 | 1.710 | 0.895 | 0.933 |
| `BDW` `BEW` — best unrestricted | 390 | 1.617 | 0.991 | 0.992 |
| `035` `64W` — best disjoint | 1,119 | 0.790 | 0.979 | 0.973 |

Maximal polarization scores eight times worse than the optimum, and `JJJ` —
indispensable in every perfect 3-fortress — is actively harmful in a pair.

What predicts pair strength is not how extreme the members are but whether the
**weaker side of the partnership is still near-total**. Over 400 random disjoint
pairs, the rank correlation with totalizers is −0.252 for polarity spread and
**−0.829** for the worse of the two type-coverages.

The reason is structural, and it mirrors part 4 exactly. A codon becomes extreme
by conceding its off-type badly: `JJJ` beats 89.1% of Flies and 4.3% of Goos. In
a triple, two partners cover that blind side, so the keystone's lopsidedness is
affordable and even useful. **In a pair there is no slack** — whatever one member
concedes, the other must cover alone — so the optimum is a balanced pair of
moderate specialists rather than a pair of extremists. Specialisation is a
three-member luxury.

### Greedy finds the optimum here, but that does not transfer

200 sampled climbs, hill-climbing exactly as part 4 does, reached **1,119 — the
proved optimum — with a gap of zero**. That is the only place in this report
where a search can be scored against a known answer.

It should not be read as a licence to trust part 4's search. The optimum's basin
is ordinary: it was reached in 6 of 200 climbs (3.0%), so greedy found it through
sheer coverage of a small space rather than through any reliable pull toward it.
At the same 3% rate on a space 10,000 times larger, part 4's triples would very
plausibly miss theirs.

### The pair landscape is smoother than the triple one

| | 2-fortress | 3-fortress |
|---|---:|---:|
| climbs | 200 | 500 |
| distinct optima | 92 | 392 |
| optima per climb | **0.46** | **0.78** |
| climbs landing in a repeated basin | **76%** | 35% |
| singleton optima | 52% | 83% |
| median steps to converge | 3 | 4 |

Pairs produce roughly half as many optima per climb and land in a repeated basin
more than twice as often, which is the concrete sense in which the smaller
problem is easier.

The largest basin belongs to `BFN` `DPM`, which attracts 11 of 200 climbs at
2,063 totalizers — nearly double the optimum. `BFN` is the strongest single codon
from part 2, so the climb is repeatedly drawn toward the individually-best codon
and settles for a mediocre pair. As with the triples, basin size and quality are
only weakly related (Spearman −0.374).

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

Part 5 is exhaustive only because of the simultaneous-permutation symmetry, so
`analysis/pair.py` asserts that symmetry rather than assuming it: for random
pairs and random position permutations it checks that the totalizer count is
unchanged. If that failed, the sweep would silently skip most of the space while
still reporting a plausible optimum. It also reproduces part 3's `{BFN, BPN}`
figure of 2,041 as a regression check on the pair counter, re-derives both
optima by replaying matchups through the engine, and asserts that no sampled
climb beat the claimed optimum. Both optima were additionally confirmed against
`game.face_off` by replaying every one of their totalizers.

Part 4 reads the same matrix and reports a count of *zero*, which is precisely
the result a transposed or mis-masked read would manufacture. `analysis/fortress.py`
therefore asserts, before printing: the best fortress's totalizer count
re-derived by replaying matchups through the engine; the candidate sweep that
drives every swap agreeing with direct per-triple counts on 20 random probes
(a wrong sweep would steer the climb while leaving the reported totals
self-consistent); every triple being at most as beatable as each pair inside it;
no member totalizing its own fortress; and every emitted triple being
character-disjoint. The three zero-totalizer fortresses were additionally
confirmed against `game.face_off` the same way: for each of the eleven, every
codon beating exactly two of its members — 22,812 to 26,970 of them — replayed
through the reference rules, with no disagreement.

## Not done

The unrestricted two-codon search is **now closed** — part 5 sweeps every pair
and returns `BDW` `BEW` at 390. What remains open for pairs is the *three*-codon
equivalent: the same symmetry reduction does not make a triple sweep tractable,
since fixing one member still leaves ~10⁹ pairs to score per first member.

For part 4, **how common perfect fortresses are** is open. Sixteen turned up in
500 climbs, but greedy from random starts gives no estimate of their density and
no bound on the gap to the global optimum. A *Goo*-side mirror of the `JJJ`
keystone is partly ruled out: every perfect fortress found polarizes against
Flies, and no Goo-polarized triple reached zero, though whether that is a real
asymmetry in the rules or a limit of this search is unresolved. Whether a
*4*-fortress can reach zero totalizers under the same no-overlap rule is also
untouched, and the disjointness
constraint caps such a set at 12 members before the 36-character alphabet is
exhausted.
