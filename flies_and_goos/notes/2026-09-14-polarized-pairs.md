# Polarized character pairs: one at the top, the other at the bottom

**Date:** 2026-09-14

## Definition

Within a contest, call a character **top** if its score is at least the
3rd-highest score present among the 36 characters (counting multiplicity), and
**bottom** if its score is at most the 3rd-lowest. Because the tables in
`src/flies_and_goos/tables.py` score coarsely (integers 0-4), these sets are
usually whole score-groups rather than literal triples - contest #7's top set
runs to 14 characters while #4's is 7. Top and bottom never overlap in any of
the 10 contests, so no contest can score a pair in both directions.

A pair is **polarized** on a contest when one member is top and the other is
bottom. The counts below are how many of the 10 contests a pair polarizes on.

| # | contest | top threshold / bottom threshold | top set | bottom set |
|---|---|---|---|---|
| 0 | stability | 3 / 0 | `12BDEILZ` | `47FPQTVY` |
| 1 | number of orthogonal line segments | 3 / 0 | `BDEFHIPR` | `03689COQSVWX` |
| 2 | number of V-junctions | 2 / 0 | `45BDEMNWZ` | `0689CGHIJKOQSTUXY` |
| 3 | number of top and bottom bumps | 2 / 0 | `03689CGMNOSW` | `7BDEFHIKLPRTXYZ` |
| 4 | number of junctions | 3 / 0 | `4ABEMRW` | `CJOSU` |
| 5 | upside-down stability | 3 / 0 | `57BDEFIPRTWZ` | `14AJL` |
| 6 | number of T-junctions plus loops tangent to the floor | 2 / 0 | `06ABHIR` | `23457CGJKLMNSUVWXYZ` |
| 7 | number of left-descending slanted line segments | 1 / 0 | `012457AKMVWXYZ` | `3689BCDEFGHIJLNOPQRSTU` |
| 8 | number of side bumps | 2 / 0 | `035689BOQS` | `1AEFHIJKLMNTUVWXYZ` |
| 9 | number of valleys | 2 / 0 | `05689BW` | `127EFILTZ` |

## Same-type pairs (curved&middot;curved or straight&middot;straight)

Of the 306 same-type pairs, 20 polarize on 4 or more contests. The cutoff
falls cleanly: nothing else ties at 4, so this is exactly a top-20.

Distribution: 6&rarr;1 5&rarr;5 4&rarr;14 3&rarr;47 2&rarr;84 1&rarr;90 0&rarr;65

| n | pair | type | polarized contests (higher &gt; lower) |
|---|---|---|---|
| 6 | `I` `W` | straight | #1 I>W, #2 W>I, #3 W>I, #6 I>W, #7 W>I, #9 W>I |
| 5 | `4` `I` | straight | #0 I>4, #2 4>I, #5 I>4, #6 I>4, #7 4>I |
| 5 | `B` `C` | curved | #1 B>C, #2 B>C, #3 C>B, #4 B>C, #6 B>C |
| 5 | `B` `J` | curved | #2 B>J, #4 B>J, #5 B>J, #6 B>J, #8 B>J |
| 5 | `B` `S` | curved | #1 B>S, #2 B>S, #3 S>B, #4 B>S, #6 B>S |
| 5 | `H` `W` | straight | #1 H>W, #2 W>H, #3 W>H, #6 H>W, #7 W>H |
| 4 | `0` `B` | curved | #1 B>0, #2 B>0, #3 0>B, #7 0>B |
| 4 | `0` `D` | curved | #1 D>0, #2 D>0, #3 0>D, #7 0>D |
| 4 | `5` `J` | curved | #2 5>J, #5 5>J, #7 5>J, #8 5>J |
| 4 | `B` `O` | curved | #1 B>O, #2 B>O, #3 O>B, #4 B>O |
| 4 | `B` `U` | curved | #2 B>U, #4 B>U, #6 B>U, #8 B>U |
| 4 | `C` `R` | curved | #1 R>C, #3 C>R, #4 R>C, #6 R>C |
| 4 | `E` `W` | straight | #1 E>W, #3 W>E, #7 W>E, #9 W>E |
| 4 | `F` `W` | straight | #1 F>W, #3 W>F, #7 W>F, #9 W>F |
| 4 | `H` `M` | straight | #2 M>H, #3 M>H, #6 H>M, #7 M>H |
| 4 | `I` `M` | straight | #2 M>I, #3 M>I, #6 I>M, #7 M>I |
| 4 | `I` `V` | straight | #0 I>V, #1 I>V, #6 I>V, #7 V>I |
| 4 | `L` `W` | straight | #3 W>L, #5 W>L, #7 W>L, #9 W>L |
| 4 | `R` `S` | curved | #1 R>S, #3 S>R, #4 R>S, #6 R>S |
| 4 | `T` `W` | straight | #2 W>T, #3 W>T, #7 W>T, #9 W>T |

## Cross-type pairs (curved&middot;straight)

Of the 324 cross-type pairs, 61 reach 4 or more - 18.8% against
6.5% for same-type pairs.

Distribution: 6&rarr;2 5&rarr;16 4&rarr;43 3&rarr;92 2&rarr;93 1&rarr;60 0&rarr;18

| n | curved | straight | polarized contests (higher &gt; lower) |
|---|---|---|---|
| 6 | `0` | `E` | #1 E>0, #2 E>0, #3 0>E, #7 0>E, #8 0>E, #9 0>E |
| 6 | `6` | `Z` | #2 Z>6, #3 6>Z, #6 6>Z, #7 Z>6, #8 6>Z, #9 6>Z |
| 5 | `0` | `F` | #1 F>0, #3 0>F, #7 0>F, #8 0>F, #9 0>F |
| 5 | `0` | `I` | #1 I>0, #3 0>I, #7 0>I, #8 0>I, #9 0>I |
| 5 | `0` | `L` | #3 0>L, #6 0>L, #7 0>L, #8 0>L, #9 0>L |
| 5 | `0` | `Z` | #2 Z>0, #3 0>Z, #6 0>Z, #8 0>Z, #9 0>Z |
| 5 | `5` | `I` | #2 5>I, #6 I>5, #7 5>I, #8 5>I, #9 5>I |
| 5 | `6` | `E` | #1 E>6, #2 E>6, #3 6>E, #8 6>E, #9 6>E |
| 5 | `8` | `E` | #1 E>8, #2 E>8, #3 8>E, #8 8>E, #9 8>E |
| 5 | `8` | `Z` | #2 Z>8, #3 8>Z, #7 Z>8, #8 8>Z, #9 8>Z |
| 5 | `9` | `E` | #1 E>9, #2 E>9, #3 9>E, #8 9>E, #9 9>E |
| 5 | `9` | `Z` | #2 Z>9, #3 9>Z, #7 Z>9, #8 9>Z, #9 9>Z |
| 5 | `B` | `V` | #0 B>V, #1 B>V, #6 B>V, #7 V>B, #8 B>V |
| 5 | `B` | `W` | #1 B>W, #3 W>B, #6 B>W, #7 W>B, #8 B>W |
| 5 | `B` | `X` | #1 B>X, #2 B>X, #6 B>X, #7 X>B, #8 B>X |
| 5 | `B` | `Y` | #0 B>Y, #2 B>Y, #6 B>Y, #7 Y>B, #8 B>Y |
| 5 | `O` | `E` | #1 E>O, #2 E>O, #3 O>E, #4 E>O, #8 O>E |
| 5 | `S` | `E` | #1 E>S, #2 E>S, #3 S>E, #4 E>S, #8 S>E |
| 4 | `0` | `H` | #1 H>0, #3 0>H, #7 0>H, #8 0>H |
| 4 | `0` | `N` | #2 N>0, #6 0>N, #7 0>N, #8 0>N |
| 4 | `0` | `T` | #3 0>T, #7 0>T, #8 0>T, #9 0>T |
| 4 | `B` | `1` | #5 B>1, #7 1>B, #8 B>1, #9 B>1 |
| 4 | `3` | `H` | #1 H>3, #3 3>H, #6 H>3, #8 3>H |
| 4 | `3` | `I` | #1 I>3, #3 3>I, #6 I>3, #8 3>I |
| 4 | `B` | `4` | #0 B>4, #5 B>4, #6 B>4, #7 4>B |
| 4 | `5` | `H` | #2 5>H, #6 H>5, #7 5>H, #8 5>H |
| 4 | `5` | `L` | #5 5>L, #7 5>L, #8 5>L, #9 5>L |
| 4 | `5` | `T` | #2 5>T, #7 5>T, #8 5>T, #9 5>T |
| 4 | `6` | `7` | #3 6>7, #6 6>7, #7 7>6, #9 6>7 |
| 4 | `6` | `F` | #1 F>6, #3 6>F, #8 6>F, #9 6>F |
| 4 | `6` | `I` | #1 I>6, #3 6>I, #8 6>I, #9 6>I |
| 4 | `6` | `K` | #3 6>K, #6 6>K, #7 K>6, #8 6>K |
| 4 | `6` | `L` | #3 6>L, #6 6>L, #8 6>L, #9 6>L |
| 4 | `6` | `M` | #2 M>6, #6 6>M, #7 M>6, #8 6>M |
| 4 | `6` | `W` | #2 W>6, #6 6>W, #7 W>6, #8 6>W |
| 4 | `6` | `X` | #3 6>X, #6 6>X, #7 X>6, #8 6>X |
| 4 | `6` | `Y` | #3 6>Y, #6 6>Y, #7 Y>6, #8 6>Y |
| 4 | `B` | `7` | #0 B>7, #6 B>7, #7 7>B, #9 B>7 |
| 4 | `8` | `F` | #1 F>8, #3 8>F, #8 8>F, #9 8>F |
| 4 | `8` | `I` | #1 I>8, #3 8>I, #8 8>I, #9 8>I |
| 4 | `9` | `F` | #1 F>9, #3 9>F, #8 9>F, #9 9>F |
| 4 | `9` | `I` | #1 I>9, #3 9>I, #8 9>I, #9 9>I |
| 4 | `S` | `A` | #4 A>S, #6 A>S, #7 A>S, #8 S>A |
| 4 | `B` | `K` | #2 B>K, #6 B>K, #7 K>B, #8 B>K |
| 4 | `B` | `L` | #5 B>L, #6 B>L, #8 B>L, #9 B>L |
| 4 | `B` | `M` | #3 M>B, #6 B>M, #7 M>B, #8 B>M |
| 4 | `B` | `T` | #0 B>T, #2 B>T, #8 B>T, #9 B>T |
| 4 | `B` | `Z` | #6 B>Z, #7 Z>B, #8 B>Z, #9 B>Z |
| 4 | `C` | `E` | #1 E>C, #2 E>C, #3 C>E, #4 E>C |
| 4 | `Q` | `E` | #0 E>Q, #1 E>Q, #2 E>Q, #8 Q>E |
| 4 | `S` | `H` | #1 H>S, #3 S>H, #6 H>S, #8 S>H |
| 4 | `S` | `I` | #1 I>S, #3 S>I, #6 I>S, #8 S>I |
| 4 | `J` | `W` | #2 W>J, #4 W>J, #5 W>J, #7 W>J |
| 4 | `O` | `M` | #2 M>O, #4 M>O, #7 M>O, #8 O>M |
| 4 | `S` | `M` | #2 M>S, #4 M>S, #7 M>S, #8 S>M |
| 4 | `O` | `W` | #2 W>O, #4 W>O, #7 W>O, #8 O>W |
| 4 | `O` | `Z` | #2 Z>O, #3 O>Z, #7 Z>O, #8 O>Z |
| 4 | `Q` | `Z` | #0 Z>Q, #2 Z>Q, #7 Z>Q, #8 Q>Z |
| 4 | `R` | `W` | #1 R>W, #3 W>R, #6 R>W, #7 W>R |
| 4 | `S` | `W` | #2 W>S, #4 W>S, #7 W>S, #8 S>W |
| 4 | `S` | `Z` | #2 Z>S, #3 S>Z, #7 Z>S, #8 S>Z |

## Observations

- **Crossing the type line polarizes far more readily.** 61/324 cross-type
  pairs reach 4+, against 20/306 same-type. The contests largely measure
  straight-line features (orthogonal segments, V-junctions, slants) against
  round-glyph features (side bumps, floor-tangent loops, valleys), so glyph type
  is itself a decent proxy for which end of a table a character lands on.
- **A few hub characters carry both lists.** Among the 61 cross-type pairs:
  `B` 12, `6` 11, `0` 8, `E` 8, `Z` 8, `I` 7, `S` 7, `W` 6. `B` and `W` are also the same-type hubs, each appearing in 6 of the 10
  pairs of their own type. A hub sits at an extreme of many contests at once -
  `B` tops #1, #2, #4, #6, #8; `W` tops #2, #3, #7, #9 - so almost any bland
  partner polarizes against it.
- **Most cross-type pairs are lopsided, not mutual.** Only 20 of the 61 are
  two-way in the sense of polarizing at least twice in each direction:
  `0E`(6), `6Z`(6), `6E`(5), `8E`(5), `8Z`(5), `9E`(5), `9Z`(5), `BW`(5), `EO`(5), `ES`(5), `3H`(4), `3I`(4), `6M`(4), `6W`(4), `BM`(4), `HS`(4), `IS`(4), `OZ`(4), `RW`(4), `SZ`(4). At the other extreme,
  8 pairs never reverse at all - one character tops the other on every
  polarized contest: `0L`, `0T`, `5L`, `5T`, `6L`, `BL`, `BT`, `JW`.
- **The strongest pair overall is cross-type and mutual.** `0`&middot;`E` and `6`&middot;`Z`
  both hit 6 of 10, each reversing direction at least twice. The best same-type
  pair, `I`&middot;`W`, also hits 6 and is genuinely bidirectional: `I` tops `W` on
  orthogonal lines and floor-loops, `W` tops `I` on V-junctions, bumps, slants
  and valleys.
- **Same-type pairs split evenly by type**: 10 curved and 10 straight among the 20.

## Caveat on the definition

With integer scores, "top 3" nearly always widens to a full score-group, so
contests with a fat extreme group (#7, #2, #6) contribute polarized contests
much more cheaply than narrow ones (#4, #9). A strict-extreme variant - only the
single maximum and minimum score-groups - would reshuffle the ranking. The
results here are for the threshold rule stated above.

## Reproduce

```python
from itertools import combinations
from flies_and_goos.tables import ALPHABET, CONTESTS, CURVED

tabs = [t for _, t in CONTESTS]
TOP, BOT = [], []
for t in tabs:
    s = sorted(t[c] for c in ALPHABET)
    TOP.append({c for c in ALPHABET if t[c] >= s[-3]})
    BOT.append({c for c in ALPHABET if t[c] <= s[2]})

for a, b in combinations(ALPHABET, 2):
    n = sum((a in TOP[i] and b in BOT[i]) or (a in BOT[i] and b in TOP[i])
            for i in range(10))
    if n >= 4:
        kind = "cross" if (a in CURVED) != (b in CURVED) else "same"
        print(n, a, b, kind)
```

