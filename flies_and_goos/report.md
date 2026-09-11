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

## Findings at a glance

| | |
|---|---|
| **Codons tying all 10 contests** | They exist — 376,924 ordered non-identical pairs. This settles the open question in `rules.docx`. |
| **Best codon** | `ENP`, winning 62.94% against a uniform opponent. Worst is `PGE` at 36.19%. |
| **Only the multiset matters** | P(win) is *exactly* invariant under permuting a codon, so the game has **8,436 distinct strategies, not 46,656**. |
| **The game is non-transitive** | `ENP`, the best codon, loses head-to-head to `PGE`, the worst. |
| **Best partner for `ENP`** | `EGP` — an arrangement of the worst multiset — defeats 88.4% of the codons that beat `ENP`. Only 4.27% of codons beat both. |
| **…but arrangement matters there** | Unlike P(win), partner coverage is *not* permutation-invariant: `EGP` covers 15,281 where `GPE` covers 12,474. |

---

## 1. Non-identical codons that tie all 10 contests

`rules.docx` says: *"It is currently unknown whether there exist non-identical
codons which tie on all 10 contests."*

**They exist, and they are common: 376,924 ordered non-identical pairs tie every
contest** — about 0.017% of all matchups.

### The reduction

A brute-force sweep is 46,656² ≈ 2.2 billion matchups. It collapses to under a
second via one observation: **relationship status cannot affect whether a
matchup is a draw.** Friend awards a position to the *lower* score and Foe to
the *higher*, so in both cases the contest is tied exactly when
`#{a < b} == #{a > b}`.

Writing `v(x,y)` for the 10-vector of `sign(score_c(x) − score_c(y))`, codons A
and B tie all 10 contests iff

```
v(a₁,b₁) + v(a₂,b₂) + v(a₃,b₃) = 0        (in Z¹⁰)
```

The 1,296 ordered character pairs carry only 889 distinct signature vectors, so
counting triples that sum to zero is 889² hash operations.

### Results

| | ordered pairs |
|---|---:|
| Tie all 10 contests | 423,580 |
| — of which identical (A = B) | 46,656 |
| — of which every position uncontested | 64,000 |
| **Non-identical ties** | **376,924** |
| **Ties where positions are actually traded** | **359,580** |

Two distinct causes:

**Interchangeable glyphs.** `6` and `9` score identically in all 10 contests, as
do `O` and `S`. (These are the *only* two such pairs in the 36-character
alphabet.) So `6AB` vs `9AB` ties by construction. This alone answers the
question, but it is a degenerate answer — the codons are never really contested.

**Genuine cancellation** — the interesting case, 359,580 ordered pairs. Every
position is contested, and the wins cancel out in all ten contests at once.
Verified witnesses (each confirmed a draw by `game.face_off`, all 10 rounds
played):

```
016 vs 160      026 vs 280      038 vs 380
061 vs 106      062 vs 208      03G vs 3G0
01L vs 1Q1      0LU vs 2T8      03H vs 3H0
0L1 vs 11Q      0UL vs 28T      03L vs 3A8
```

Note `016 vs 160` and `061 vs 106`: a pure **permutation** of the same three
characters can draw with itself. Since each player's stability is unchanged by
permutation, both start at the same contest, and the position-by-position wins
happen to cancel in every one.

Ties remain rare per codon: a codon draws with 9.1 opponents on average out of
46,656, and every codon has at least one draw (itself). The most draw-prone is
`66I` with 68 — unsurprising, since its `6`s can each be swapped for `9`.

---

## 2. Best codon against a uniformly random opponent

Full 46,656 × 46,656 sweep, 2.18 billion matchups, ranked by P(win).

### The winner: `ENP` (and its permutations), P(win) = 0.6294

| codon | type | stab | P(win) | P(loss) | draws |
|---|---|---:|---:|---:|---:|
| ENP | Fly | 5 | 0.6294 | 0.3705 | 4 |
| NIP | Fly | 5 | 0.6259 | 0.3739 | 6 |
| 4ER | Fly | 5 | 0.6244 | 0.3755 | 5 |
| E4G | Fly | 5 | 0.6234 | 0.3764 | 9 |
| 4IR | Fly | 5 | 0.6229 | 0.3771 | 4 |
| FDN | Fly | 5 | 0.6226 | 0.3773 | 4 |
| DFH | Fly | 5 | 0.6213 | 0.3786 | 4 |
| QHE | Fly | 5 | 0.6209 | 0.3788 | 12 |

Worst codons:

| codon | type | stab | P(win) | P(loss) |
|---|---|---:|---:|---:|
| PGE | Goo | 5 | 0.3619 | 0.6378 |
| REQ | Goo | 5 | 0.3640 | 0.6358 |
| RDF | Goo | 5 | 0.3650 | 0.6349 |
| EPR | Goo | 5 | 0.3669 | 0.6331 |

The full ranking of all 46,656 codons is in `winrates.tsv` (gitignored;
regenerate with the command above).

### Only the multiset of characters matters

**P(win) is exactly invariant under permuting a codon's characters.** All 46,656
codons fall into 8,436 multiset classes, and within every class the win, loss
and draw counts are identical — verified, not sampled.

The reason: contests compare position-by-position, so playing a permuted codon
`A∘π` against opponent `B` is the same as playing `A` against `B∘π⁻¹`. Stability
and Fly/Goo type are both permutation-invariant, so the relationship and the
opening contest are unchanged, and permuting a uniformly random opponent leaves
it uniformly random. `ENP`, `EPN`, `NEP`, `NPE`, `PEN`, `PNE` are one strategy,
not six.

**So there are only 8,436 genuinely distinct choices in this game**, not 46,656.

### Flies have a real edge

| type | n | mean P(win) | best | worst |
|---|---:|---:|---:|---:|
| Fly | 23,328 | 0.5118 | 0.6294 (`ENP`) | 0.3845 |
| Goo | 23,328 | 0.4880 | 0.5935 (`88A`) | 0.3619 |

Being a Fly is worth about 2.4 percentage points on average, and the top 15
codons are all Flies. Note this is not a counting artifact: the type split is
exactly even (23,328 each), since a codon is a Fly when at least 2 of its 3
characters are straight and the alphabet is 18/18 straight/curved. The advantage
comes from how Fly score profiles fare under the Friend/Foe flip, not from there
being more Flies.

### Stability 5 is where the extremes live

Mean P(win) is nearly flat across stability (0.478 to 0.508), but **every one of
the top 15 and bottom 8 codons has stability 5**:

| stability | n | mean P(win) | best |
|---:|---:|---:|---|
| 0 | 512 | 0.4780 | 0.5292 (`PPP`) |
| 1 | 2,112 | 0.5008 | 0.5891 (`445`) |
| 2 | 4,632 | 0.4967 | 0.5872 (`08P`) |
| 3 | 7,619 | 0.4936 | 0.5820 (`4LP`) |
| 4 | 9,435 | 0.5080 | 0.5988 (`4MM`) |
| **5** | **9,033** | 0.4974 | **0.6294 (`ENP`)** |
| 6 | 7,017 | 0.5035 | 0.5710 (`8BG`) |
| 7 | 4,056 | 0.5018 | 0.5651 (`8BL`) |
| 8 | 1,728 | 0.4943 | 0.5657 (`22W`) |
| 9 | 512 | 0.4902 | 0.5371 (`22I`) |

This is not a sample-size artifact — stability 4 is the larger class (9,435
codons) yet tops out 3 points lower. Stability enters twice, as contest #0's
score and as the selector for the opening contest, and stability 5 apparently
aligns a codon's own strengths with the contests it lands in. The precise
mechanism is not established here.

### The game is non-transitive

**`ENP` loses head-to-head to `PGE`** — the best codon in the game loses to the
worst. Being strong against the field is not the same as being strong against
any particular opponent, so there is no codon that simply dominates, and a
player who knows their opponent's tendencies should not just play `ENP`.

### Distribution

P(win) spans 0.3619 to 0.6294 with median 0.5018; 51.9% of codons are above
0.5. Codon choice is worth real money — the best choice wins roughly 63% against
a random opponent versus the worst choice's 36% — but the spread is narrow
enough, and the non-transitivity real enough, that Phil can still get lucky.

---

## 3. The best partner for `ENP`

`ENP` wins most often, but 17,287 codons (37.05%) still beat it. Which single
codon cleans up the most of that set?

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py   # ~30s
```

### The answer: `EGP`, which defeats 15,281 of ENP's 17,287 conquerors (88.4%)

| partner | covers | frac | beat both | partner's own P(win) |
|---|---:|---:|---:|---:|
| **EGP** | **15,281** | **0.8840** | **1,994** | 0.3619 |
| ERP | 14,876 | 0.8605 | 2,410 | 0.3669 |
| DRF | 14,376 | 0.8316 | 2,907 | 0.3650 |
| ZRP | 14,293 | 0.8268 | 2,990 | 0.3835 |
| LRP | 14,253 | 0.8245 | 3,029 | 0.3724 |
| 1RP | 14,128 | 0.8173 | 3,155 | 0.3777 |
| 071 | 14,108 | 0.8161 | 3,179 | 0.5298 |

**The best partner for the best codon is the worst codon in the game.** `EGP` is
an arrangement of the `EGP` multiset — precisely the multiset with the lowest
P(win) of all 8,436 (0.3619, the `PGE` row in part 2). The non-transitivity
noted above is not a curiosity; it is exploitable.

`EGP` is simultaneously the *global* minimiser of "codons that beat both", at
1,994. The two objectives pick the same codon, as expected — coverage and shared
losses sum to 17,287 minus the handful of draws.

### As a pair, `{ENP, EGP}` is very hard to sweep

| | beaten by |
|---|---:|
| `ENP` alone | 37.05% of codons |
| **`{ENP, EGP}` both** | **4.27%** of codons |

Only 1,994 of 46,656 codons beat both. (This is the deferred two-codon question
restricted to `ENP` as one member, so it is an upper bound on the best duo
overall, not necessarily the global optimum.)

### If you want both halves playable: `071`

Every top partner except one is a weak codon in its own right. `071` is the
exception — P(win) 0.5298, above average, while still covering 81.6% of ENP's
conquerors. `{ENP, 071}` is beaten by 6.81% of the field, against 4.27% for
`{ENP, EGP}`, which buys a genuinely competitive second codon.

### Partner quality is unrelated to codon quality

Tempting conclusion from the table above: weak codons make good partners.
**That is false in general** — the correlation between coverage and P(win) across
all 46,656 candidates is −0.0098, essentially zero. Weak codons crowd the very
top of the partner ranking, but being weak predicts nothing about being a useful
partner. The worst partners are also mostly strong codons (`EMP`, 819; `ENF`,
1,055 — both above 0.58 P(win)), which is the same phenomenon: codons that
resemble `ENP` lose to the same opponents it does.

### Arrangement matters here, unlike P(win)

Part 2 showed P(win) is exactly permutation-invariant. **Coverage is not:**

| arrangement | covers |
|---|---:|
| EGP | 15,281 |
| PGE | 13,667 |
| EPG | 13,233 |
| GEP | 12,547 |
| PEG | 12,488 |
| GPE | 12,474 |

A 2,807-codon spread across one multiset. The invariance proof in part 2 relied
on the opponent being *uniformly* random, so that permuting it changes nothing.
`L(ENP)` is a fixed, arbitrary set with no such symmetry, so the specific
arrangement matters — picking `GPE` instead of `EGP` throws away a sixth of the
coverage. Choosing a partner is a genuinely 46,656-way choice, not an 8,436-way
one.

## Correctness

Everything above rests on `flies_and_goos.engine`, a vectorized reimplementation
of the rules. It is checked against the readable reference implementation
(`flies_and_goos.game`, which passes the 10 supplied test cases) by:

- agreement on all 10 supplied cases and 2,000 pseudo-random matchups;
- antisymmetry, `outcome(A,B) == −outcome(B,A)`;
- per-codon stability and Fly/Goo type matching `game.Codon`;
- wins summed over all codons equalling losses summed over all codons;
- **an independent cross-check**: the full sweep counts 423,580 drawn matchups,
  exactly the number the signature-sum method of part 1 derives without playing
  a single game. Two unrelated algorithms agreeing on that figure is the
  strongest evidence here that both are correct.

Part 3 reads the bit-packed beat-matrix, where a transposed read would give a
plausible but wrong answer, so `analysis/partner.py` re-derives the top 10
partners by replaying those matchups through the engine and asserts agreement
before printing anything. It also checks `coverage[ENP] == 0` (a codon cannot
defeat anyone who defeats it) and that mean coverage lands near half the target
set. `tests/test_duo.py` repeats these; they skip when `beat_matrix.npy` has not
been built.

## Not done

The **unrestricted** two-codon search — the best duo over all ~1.09 billion
pairs, rather than pairs containing `ENP` — is still open. Part 3 gives the best
`ENP` duo at 4.27%, which upper-bounds the global optimum but does not attain
it. `flies_and_goos.duo.coverage_counts` is written generically over a target
set, so it is the building block for that sweep.
