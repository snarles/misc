# Character similarity: pairs tying 7+ of 10 contests

**Date:** 2026-09-14

## Definition

Each of the 36 codon characters (`0-9A-Z`) gets one integer score per contest
from the 10 tables in `src/flies_and_goos/tables.py`. Define the *similarity* of
two characters as the number of contests on which their scores are equal. This
is a per-character property only — it says nothing directly about codon matchups,
since a face-off picks contests based on both codons' stability and type.

Computed over all C(36,2) = 630 pairs. **35 pairs tie on 7 or more contests.**
No pair ties all 10, which independently confirms the `report.md` claim that no
two characters are functionally identical.

| similarity | pairs |
|---|---|
| 10/10 | 0 |
| 9/10  | 6 |
| 8/10  | 8 |
| 7/10  | 21 |

## 9/10 — a single differing contest

| pair | differs only on |
|---|---|
| `6` `9` | #6 T-junctions + floor-tangent loops |
| `8` `9` | #8 side bumps |
| `C` `S` | #8 side bumps |
| `K` `X` | #1 orthogonal line segments |
| `K` `Y` | #0 stability |
| `O` `S` | #6 T-junctions + floor-tangent loops |

## 8/10 — two differing contests

| pair | differs on |
|---|---|
| `3` `S` | #2 V-junctions, #4 junctions |
| `6` `8` | #6 floor loops, #8 side bumps |
| `9` `O` | #4 junctions, #9 valleys |
| `C` `O` | #6 floor loops, #8 side bumps |
| `D` `P` | #0 stability, #2 V-junctions |
| `F` `P` | #8 side bumps, #9 valleys |
| `J` `U` | #1 orthogonal lines, #5 upside-down stability |
| `X` `Y` | #0 stability, #1 orthogonal lines |

## 7/10 — three differing contests

| pair | differs on |
|---|---|
| `0` `6` | #4 junctions, #6 floor loops, #7 left-descending slants |
| `0` `9` | #4 junctions, #6 floor loops, #7 left-descending slants |
| `2` `7` | #0 stability, #3 top/bottom bumps, #5 upside-down stability |
| `3` `6` | #2 V-junctions, #6 floor loops, #9 valleys |
| `3` `9` | #2 V-junctions, #6 floor loops, #9 valleys |
| `3` `C` | #2 V-junctions, #4 junctions, #8 side bumps |
| `3` `O` | #2 V-junctions, #4 junctions, #6 floor loops |
| `6` `O` | #4 junctions, #6 floor loops, #9 valleys |
| `6` `S` | #4 junctions, #6 floor loops, #9 valleys |
| `8` `O` | #4 junctions, #8 side bumps, #9 valleys |
| `9` `Q` | #0 stability, #3 top/bottom bumps, #9 valleys |
| `9` `S` | #4 junctions, #6 floor loops, #9 valleys |
| `C` `G` | #0 stability, #1 orthogonal lines, #4 junctions |
| `F` `I` | #0 stability, #2 V-junctions, #6 floor loops |
| `F` `T` | #1 orthogonal lines, #2 V-junctions, #4 junctions |
| `H` `I` | #0 stability, #5 upside-down stability, #9 valleys |
| `M` `N` | #2 V-junctions, #4 junctions, #7 left-descending slants |
| `O` `Q` | #0 stability, #3 top/bottom bumps, #4 junctions |
| `P` `R` | #0 stability, #4 junctions, #6 floor loops |
| `V` `X` | #0 stability, #2 V-junctions, #3 top/bottom bumps |
| `V` `Y` | #1 orthogonal lines, #2 V-junctions, #3 top/bottom bumps |

## Observations

- **The round cluster dominates.** `0 3 6 8 9 C O Q S` account for 20 of the 35
  pairs. Contests #4 (junctions), #6 (T-junctions + floor loops), and #9
  (valleys) are what keep these characters distinguishable at all; drop those
  three and most of the cluster collapses together.
- **Letters only** (dropping digits) leaves 18 pairs: `C·S`, `K·X`, `K·Y`, `O·S`
  at 9/10; `C·O`, `D·P`, `F·P`, `J·U`, `X·Y` at 8/10; `C·G`, `F·I`, `F·T`,
  `H·I`, `M·N`, `O·Q`, `P·R`, `V·X`, `V·Y` at 7/10.
- **The near-misses are geometrically sensible.** `K/X`, `X/Y`, `V/X`, `V/Y`,
  `M/N` are straight-line glyphs separated by one slant or junction count;
  `C/S`, `C/O`, `O/S`, `6/8/9` are curved glyphs separated by whether the
  enclosed space touches the floor or produces a side bump.
- **Contests #4 and #6 are the load-bearing distinguishers** in this set,
  appearing in 15 and 14 of the 35 pairs' difference sets respectively; #6 is
  the sole difference for `6·9` and `O·S`. Full breakdown of how many of the 35
  pairs each contest separates: #4: 15, #6: 14, #0: 11, #2: 11, #9: 10, #8: 7,
  #1: 6, #3: 5, #7: 3, #5: 3.

## Reproduce

```python
from itertools import combinations
from flies_and_goos.tables import ALPHABET, CONTESTS

tabs = [t for _, t in CONTESTS]
for a, b in combinations(ALPHABET, 2):
    ties = sum(t[a] == t[b] for t in tabs)
    if ties >= 7:
        print(ties, a, b)
```
