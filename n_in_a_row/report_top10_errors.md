# Top-10 worst and top-10 best linear-model predictions

## 1. What this report is

Each section below shows one canonical state from `error_analysis_top100.tsv`, picked from the rows that the natural-distribution weighted OLS (7-feature, matrix only) does worst and best on. The states were re-evaluated at high precision (n = 125,000 rollouts each), so the empirical win probability is essentially noise-free at this scale. Predictions come from the natural-dataset 7-feature and 11-feature regressions in `report_natural.md`.

Selection metric: `|empirical − pred_7|`, sorted descending. The first 50 rows of `error_analysis_top100.tsv` are the worst 50; the last 50 are the best 50 (descending order, so row 100 is the very best). Here we show only the top 10 of each.

## 2. Headline

The 11-feature model **3.3× shrinks** the mean absolute error on the worst 10 states (0.248 → 0.076), primarily by capturing immediate-win-cell saturation. On the best 10, both models are nearly perfect (0.018 → 0.020, 0.9× shrink). The dominant remaining failure mode — race conditions where both sides are one cell from winning — would need interaction terms or a non-linear model to fix.

## 3. Failure-mode incidence (top 20 worst)

I scanned the top 20 worst rows (not just 10) to get more reliable incidence rates. Each row may match multiple modes:

| failure mode | incidence (out of 20) |
|---|---|
| Late-game (≥ 8 tokens placed) | 19 / 20 |
| Race condition (both sides have ≥ 1 imm-win cell) | 17 / 20 |
| Threat saturation, own side (`m20 > imm_win_to_move`) | 12 / 20 |
| Threat saturation, opp side (`m02 > imm_win_opp`) | 8 / 20 |
| 7-feat prediction outside [0, 1] | 7 / 20 |
| 11-feat prediction outside [0, 1] | 5 / 20 |
| Asymmetric material (`|n_to_move − n_opp| ≥ 3`) | 7 / 20 |

**The dominant failure mode is the race condition** — neither model can express the structural fact that whoever fires first this ply wins. Saturation on the dominant side is also common (12/20 own, 8/20 opp); it is exactly what the `imm_win_*` features were added to fix in the 11-feature model, and it is where the 11-feat model outperforms 7-feat the most.

## 4. Per-state error summary

| group | mean \|err\| (7-feat) | mean \|err\| (11-feat) | 11-feat shrink |
|---|---|---|---|
| worst 10 | 0.2478 | 0.0758 | 3.27× |
| best 10  | 0.0177 | 0.0198 | 0.90× |

## 5. Worst 10 states (worst first)

### Worst #1: state = (`BCEFHKO`, `DIM`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
. X X O
X X . X
O . X .
O . X .
```

- **Empirical** P(to-move wins) = **0.9381**  (n = 125,000, n_wins = 117,264)
- **7-feat** prediction = `+1.3176`  →  error = `-0.3794`
- **11-feat** prediction = `+1.0438`  →  error = `-0.1057`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 0 | 5 | 4 | 1 | 10 | 2 | 7 | 3 | 5 | 0 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 2 | -0.0173 | -0.0346 |
| `m02` | 0 | -0.0594 | -0.0000 |
| `m10` | 5 | +0.0175 | +0.0875 |
| `m11` | 4 | -0.0004 | -0.0016 |
| `m12` | 1 | -0.0054 | -0.0054 |
| `m20` | 10 | +0.0723 | +0.7230 |
| `m21` | 2 | +0.0031 | +0.0062 |
| **total** |  |  | **+1.3177** |

**Failure analysis:**

- **Saturation, own side.** `m20 = 10` but only `5` distinct winning cells (line/cell ratio 2.0). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.318` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **11-feat extrapolation.** Prediction `+1.044` is also outside [0, 1] despite the `imm_win_*` features; high token counts push the linear sum past the boundary.
- **Late-game state** (10 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **Asymmetric material** (`n_to_move = 7`, `n_opp = 3`, diff `+4`). The model has no tempo-recovery feature; the side with more material has more removable cells the opponent could land on, but linear coefficients can't represent that branching.
- **11-feat error shrink.** `|err_7| = 0.379` → `|err_11| = 0.106` (ratio 3.6×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #2: state = (`ABELP`, `CFHJKMO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X X O .
X O . O
. O O X
O . O X
```

- **Empirical** P(to-move wins) = **0.3936**  (n = 125,000, n_wins = 49,198)
- **7-feat** prediction = `+0.1124`  →  error = `+0.2811`
- **11-feat** prediction = `+0.3771`  →  error = `+0.0165`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 9 | 0 | 5 | 5 | 2 | 2 | 5 | 7 | 2 | 3 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 1 | -0.0173 | -0.0173 |
| `m02` | 9 | -0.0594 | -0.5346 |
| `m10` | 0 | +0.0175 | +0.0000 |
| `m11` | 5 | -0.0004 | -0.0020 |
| `m12` | 5 | -0.0054 | -0.0270 |
| `m20` | 2 | +0.0723 | +0.1446 |
| `m21` | 2 | +0.0031 | +0.0062 |
| **total** |  |  | **+0.1125** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 2`, `imm_win_opp = 3`). To-move's per-turn win chance is `2/16 = 12.5%`; if to-move misses, opp's chance next ply is `3/16 = 18.8%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, opp side.** `m02 = 9` but only `3` distinct opp-winning cells (ratio 3.0).
- **Late-game state** (12 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.281` → `|err_11| = 0.016` (ratio 17.1×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #3: state = (`ACFHJLMO`, `BEP`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X O X .
O X . X
. X . X
X . X O
```

- **Empirical** P(to-move wins) = **0.9562**  (n = 125,000, n_wins = 119,528)
- **7-feat** prediction = `+1.2180`  →  error = `-0.2618`
- **11-feat** prediction = `+1.0765`  →  error = `-0.1202`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 5 | 7 | 0 | 8 | 4 | 8 | 3 | 5 | 0 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 0 | -0.0173 | -0.0000 |
| `m02` | 0 | -0.0594 | -0.0000 |
| `m10` | 5 | +0.0175 | +0.0875 |
| `m11` | 7 | -0.0004 | -0.0028 |
| `m12` | 0 | -0.0054 | -0.0000 |
| `m20` | 8 | +0.0723 | +0.5784 |
| `m21` | 4 | +0.0031 | +0.0124 |
| **total** |  |  | **+1.2181** |

**Failure analysis:**

- **Saturation, own side.** `m20 = 8` but only `5` distinct winning cells (line/cell ratio 1.6). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.218` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **11-feat extrapolation.** Prediction `+1.076` is also outside [0, 1] despite the `imm_win_*` features; high token counts push the linear sum past the boundary.
- **Late-game state** (11 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **Asymmetric material** (`n_to_move = 8`, `n_opp = 3`, diff `+5`). The model has no tempo-recovery feature; the side with more material has more removable cells the opponent could land on, but linear coefficients can't represent that branching.
- **11-feat error shrink.** `|err_7| = 0.262` → `|err_11| = 0.120` (ratio 2.2×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #4: state = (`BEGHJK`, `ACIMN`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
O X O .
X . X X
O X X .
O O . .
```

- **Empirical** P(to-move wins) = **0.7581**  (n = 125,000, n_wins = 94,768)
- **7-feat** prediction = `+1.0323`  →  error = `-0.2741`
- **11-feat** prediction = `+0.7918`  →  error = `-0.0337`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 2 | 3 | 3 | 3 | 8 | 4 | 6 | 5 | 4 | 2 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 1 | -0.0173 | -0.0173 |
| `m02` | 2 | -0.0594 | -0.1188 |
| `m10` | 3 | +0.0175 | +0.0525 |
| `m11` | 3 | -0.0004 | -0.0012 |
| `m12` | 3 | -0.0054 | -0.0162 |
| `m20` | 8 | +0.0723 | +0.5784 |
| `m21` | 4 | +0.0031 | +0.0124 |
| **total** |  |  | **+1.0324** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 4`, `imm_win_opp = 2`). To-move's per-turn win chance is `4/16 = 25.0%`; if to-move misses, opp's chance next ply is `2/16 = 12.5%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 8` but only `4` distinct winning cells (line/cell ratio 2.0). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.032` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **Late-game state** (11 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.274` → `|err_11| = 0.034` (ratio 8.1×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #5: state = (`ABFGIO`, `DMNP`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X X . O
. X X .
X . . .
O O X O
```

- **Empirical** P(to-move wins) = **0.9289**  (n = 125,000, n_wins = 116,113)
- **7-feat** prediction = `+1.1804`  →  error = `-0.2515`
- **11-feat** prediction = `+1.0395`  →  error = `-0.1106`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 3 | 0 | 3 | 6 | 2 | 9 | 0 | 6 | 4 | 6 | 0 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 3 | -0.0173 | -0.0519 |
| `m02` | 0 | -0.0594 | -0.0000 |
| `m10` | 3 | +0.0175 | +0.0525 |
| `m11` | 6 | -0.0004 | -0.0024 |
| `m12` | 2 | -0.0054 | -0.0108 |
| `m20` | 9 | +0.0723 | +0.6507 |
| `m21` | 0 | +0.0031 | +0.0000 |
| **total** |  |  | **+1.1807** |

**Failure analysis:**

- **Saturation, own side.** `m20 = 9` but only `6` distinct winning cells (line/cell ratio 1.5). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.180` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **11-feat extrapolation.** Prediction `+1.039` is also outside [0, 1] despite the `imm_win_*` features; high token counts push the linear sum past the boundary.
- **Late-game state** (10 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.252` → `|err_11| = 0.111` (ratio 2.3×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #6: state = (`BFGIK`, `HNP`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
. X . .
. X X O
X . X .
. O . O
```

- **Empirical** P(to-move wins) = **0.8387**  (n = 125,000, n_wins = 104,842)
- **7-feat** prediction = `+1.0906`  →  error = `-0.2518`
- **11-feat** prediction = `+0.9973`  →  error = `-0.1586`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 2 | 7 | 1 | 1 | 8 | 2 | 5 | 3 | 6 | 2 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 2 | -0.0173 | -0.0346 |
| `m02` | 2 | -0.0594 | -0.1188 |
| `m10` | 7 | +0.0175 | +0.1225 |
| `m11` | 1 | -0.0004 | -0.0004 |
| `m12` | 1 | -0.0054 | -0.0054 |
| `m20` | 8 | +0.0723 | +0.5784 |
| `m21` | 2 | +0.0031 | +0.0062 |
| **total** |  |  | **+1.0905** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 6`, `imm_win_opp = 2`). To-move's per-turn win chance is `6/16 = 37.5%`; if to-move misses, opp's chance next ply is `2/16 = 12.5%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 8` but only `6` distinct winning cells (line/cell ratio 1.3). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.091` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **Late-game state** (8 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.252` → `|err_11| = 0.159` (ratio 1.6×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #7: state = (`ACFGNO`, `ILM`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . X .
. X X .
O . . O
O X X .
```

- **Empirical** P(to-move wins) = **0.8715**  (n = 125,000, n_wins = 108,941)
- **7-feat** prediction = `+1.1024`  →  error = `-0.2309`
- **11-feat** prediction = `+1.0228`  →  error = `-0.1513`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 4 | 1 | 6 | 3 | 0 | 8 | 2 | 6 | 3 | 6 | 1 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 4 | -0.0173 | -0.0692 |
| `m02` | 1 | -0.0594 | -0.0594 |
| `m10` | 6 | +0.0175 | +0.1050 |
| `m11` | 3 | -0.0004 | -0.0012 |
| `m12` | 0 | -0.0054 | -0.0000 |
| `m20` | 8 | +0.0723 | +0.5784 |
| `m21` | 2 | +0.0031 | +0.0062 |
| **total** |  |  | **+1.1024** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 6`, `imm_win_opp = 1`). To-move's per-turn win chance is `6/16 = 37.5%`; if to-move misses, opp's chance next ply is `1/16 = 6.2%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 8` but only `6` distinct winning cells (line/cell ratio 1.3). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **7-feat extrapolation.** Prediction `+1.102` lies outside [0, 1]; OLS has no saturation mechanism for `m20` / `m02`.
- **11-feat extrapolation.** Prediction `+1.023` is also outside [0, 1] despite the `imm_win_*` features; high token counts push the linear sum past the boundary.
- **Late-game state** (9 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **Asymmetric material** (`n_to_move = 6`, `n_opp = 3`, diff `+3`). The model has no tempo-recovery feature; the side with more material has more removable cells the opponent could land on, but linear coefficients can't represent that branching.
- **11-feat error shrink.** `|err_7| = 0.231` → `|err_11| = 0.151` (ratio 1.5×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #8: state = (`ABDEJK`, `GHLO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X X . X
X . O O
. X X O
. . O .
```

- **Empirical** P(to-move wins) = **0.6779**  (n = 125,000, n_wins = 84,734)
- **7-feat** prediction = `+0.8669`  →  error = `-0.1890`
- **11-feat** prediction = `+0.6910`  →  error = `-0.0131`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 2 | 3 | 4 | 3 | 6 | 3 | 6 | 4 | 3 | 2 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 2 | -0.0173 | -0.0346 |
| `m02` | 2 | -0.0594 | -0.1188 |
| `m10` | 3 | +0.0175 | +0.0525 |
| `m11` | 4 | -0.0004 | -0.0016 |
| `m12` | 3 | -0.0054 | -0.0162 |
| `m20` | 6 | +0.0723 | +0.4338 |
| `m21` | 3 | +0.0031 | +0.0093 |
| **total** |  |  | **+0.8670** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 3`, `imm_win_opp = 2`). To-move's per-turn win chance is `3/16 = 18.8%`; if to-move misses, opp's chance next ply is `2/16 = 12.5%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 6` but only `3` distinct winning cells (line/cell ratio 2.0). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **Late-game state** (10 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.189` → `|err_11| = 0.013` (ratio 14.4×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #9: state = (`ACGJLO`, `DEHN`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . X O
O . X O
. X . X
. O X .
```

- **Empirical** P(to-move wins) = **0.7708**  (n = 125,000, n_wins = 96,356)
- **7-feat** prediction = `+0.9672`  →  error = `-0.1963`
- **11-feat** prediction = `+0.7927`  →  error = `-0.0219`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 1 | 4 | 8 | 1 | 6 | 2 | 6 | 4 | 3 | 1 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 1 | -0.0173 | -0.0173 |
| `m02` | 1 | -0.0594 | -0.0594 |
| `m10` | 4 | +0.0175 | +0.0700 |
| `m11` | 8 | -0.0004 | -0.0032 |
| `m12` | 1 | -0.0054 | -0.0054 |
| `m20` | 6 | +0.0723 | +0.4338 |
| `m21` | 2 | +0.0031 | +0.0062 |
| **total** |  |  | **+0.9673** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 3`, `imm_win_opp = 1`). To-move's per-turn win chance is `3/16 = 18.8%`; if to-move misses, opp's chance next ply is `1/16 = 6.2%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 6` but only `3` distinct winning cells (line/cell ratio 2.0). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **Late-game state** (10 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.196` → `|err_11| = 0.022` (ratio 9.0×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

### Worst #10: state = (`ABGHKP`, `DELNO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X X . O
O . X X
. . X O
. O O X
```

- **Empirical** P(to-move wins) = **0.6403**  (n = 125,000, n_wins = 80,038)
- **7-feat** prediction = `+0.8027`  →  error = `-0.1624`
- **11-feat** prediction = `+0.6138`  →  error = `+0.0265`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 2 | 3 | 5 | 2 | 5 | 4 | 6 | 5 | 2 | 2 |

**7-feature prediction decomposition** (intercept + Σ coef · feature):

| term | feature value | coef | contribution |
|---|---|---|---|
| `intercept` | 1 | +0.5426 | +0.5426 |
| `m01` | 2 | -0.0173 | -0.0346 |
| `m02` | 2 | -0.0594 | -0.1188 |
| `m10` | 3 | +0.0175 | +0.0525 |
| `m11` | 5 | -0.0004 | -0.0020 |
| `m12` | 2 | -0.0054 | -0.0108 |
| `m20` | 5 | +0.0723 | +0.3615 |
| `m21` | 4 | +0.0031 | +0.0124 |
| **total** |  |  | **+0.8028** |

**Failure analysis:**

- **Race condition.** Both sides one cell from winning (`imm_win_to_move = 2`, `imm_win_opp = 2`). To-move's per-turn win chance is `2/16 = 12.5%`; if to-move misses, opp's chance next ply is `2/16 = 12.5%`. The linear model treats own and opp threats as additive effects; the true outcome turns on which side fires first.
- **Saturation, own side.** `m20 = 5` but only `2` distinct winning cells (line/cell ratio 2.5). Linear `m20` credits each line independently; lines sharing a cell add no extra winning chance.
- **Late-game state** (11 tokens placed). Removal dynamics matter — one drawn cell can flip an `m02` → `m01` or an `m21` → `m20` — none of which a static feature vector sees.
- **11-feat error shrink.** `|err_7| = 0.162` → `|err_11| = 0.026` (ratio 6.1×). The `imm_win_*` features capture saturation that `m20`/`m02` alone cannot.

## 6. Best 10 states (best last; Best #1 = the very best)

### Best #10: state = (`BF`, `ACO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
O X O .
. X . .
. . . .
. . O .
```

- **Empirical** P(to-move wins) = **0.6087**  (n = 125,000, n_wins = 76,091)
- **7-feat** prediction = `+0.5920`  →  error = `+0.0168`
- **11-feat** prediction = `+0.5968`  →  error = `+0.0119`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 6 | 0 | 5 | 3 | 1 | 1 | 0 | 2 | 3 | 1 | 0 |

Mid-game state (5 tokens). Threat saturation: own matches (`m20 = 1`, `imm_win_to_move = 1`); opp matches (`m02 = 0`, `imm_win_opp = 0`). Both predictions in / in `[0, 1]`.

### Best #9: state = (`ADJO`, `CHKL`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . O X
. . . O
. X O O
. . X .
```

- **Empirical** P(to-move wins) = **0.5057**  (n = 125,000, n_wins = 63,217)
- **7-feat** prediction = `+0.5320`  →  error = `-0.0263`
- **11-feat** prediction = `+0.5293`  →  error = `-0.0235`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 4 | 3 | 6 | 5 | 2 | 2 | 0 | 4 | 4 | 2 | 3 |

Mid-game state (8 tokens). Threat saturation: own matches (`m20 = 2`, `imm_win_to_move = 2`); opp matches (`m02 = 3`, `imm_win_opp = 3`). Both predictions in / in `[0, 1]`.

### Best #8: state = (`AF`, `CNO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . O .
. X . .
. . . .
. O O .
```

- **Empirical** P(to-move wins) = **0.4597**  (n = 125,000, n_wins = 57,468)
- **7-feat** prediction = `+0.4960`  →  error = `-0.0362`
- **11-feat** prediction = `+0.4874`  →  error = `-0.0277`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 5 | 2 | 5 | 3 | 0 | 1 | 0 | 2 | 3 | 1 | 2 |

Mid-game state (5 tokens). Threat saturation: own matches (`m20 = 1`, `imm_win_to_move = 1`); opp matches (`m02 = 2`, `imm_win_opp = 2`). Both predictions in / in `[0, 1]`.

### Best #7: state = (`AFH`, `I`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . . .
. X . X
O . . .
. . . .
```

- **Empirical** P(to-move wins) = **0.7985**  (n = 125,000, n_wins = 99,811)
- **7-feat** prediction = `+0.7920`  →  error = `+0.0065`
- **11-feat** prediction = `+0.8113`  →  error = `-0.0128`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 0 | 8 | 2 | 0 | 2 | 0 | 3 | 1 | 2 | 0 |

Mid-game state (4 tokens). Threat saturation: own matches (`m20 = 2`, `imm_win_to_move = 2`); opp matches (`m02 = 0`, `imm_win_opp = 0`). Both predictions in / in `[0, 1]`.

### Best #6: state = (`BCJ`, `AEFO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
O X X .
O O . .
. X . .
. . O .
```

- **Empirical** P(to-move wins) = **0.4516**  (n = 125,000, n_wins = 56,450)
- **7-feat** prediction = `+0.4380`  →  error = `+0.0136`
- **11-feat** prediction = `+0.4242`  →  error = `+0.0274`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 6 | 3 | 6 | 2 | 1 | 1 | 2 | 3 | 4 | 1 | 3 |

Mid-game state (7 tokens). Threat saturation: own matches (`m20 = 1`, `imm_win_to_move = 1`); opp matches (`m02 = 3`, `imm_win_opp = 3`). Both predictions in / in `[0, 1]`.

### Best #5: state = (`AHK`, `DEFIN`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X . . O
O O . X
O . X .
. O . .
```

- **Empirical** P(to-move wins) = **0.2706**  (n = 125,000, n_wins = 33,819)
- **7-feat** prediction = `+0.2880`  →  error = `-0.0174`
- **11-feat** prediction = `+0.2567`  →  error = `+0.0138`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 6 | 4 | 5 | 4 | 1 | 0 | 2 | 3 | 5 | 0 | 4 |

Mid-game state (8 tokens). Threat saturation: own matches (`m20 = 0`, `imm_win_to_move = 0`); opp matches (`m02 = 4`, `imm_win_opp = 4`). Both predictions in / in `[0, 1]`.

### Best #4: state = (`ABO`, `CDGM`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
X X O O
. . O .
. . . .
O . X .
```

- **Empirical** P(to-move wins) = **0.3697**  (n = 125,000, n_wins = 46,215)
- **7-feat** prediction = `+0.3620`  →  error = `+0.0077`
- **11-feat** prediction = `+0.3929`  →  error = `-0.0232`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 5 | 3 | 5 | 3 | 1 | 0 | 1 | 3 | 4 | 0 | 2 |

Mid-game state (7 tokens). Threat saturation: own matches (`m20 = 0`, `imm_win_to_move = 0`); opp approximate (`m02 = 3`, `imm_win_opp = 2`). Both predictions in / in `[0, 1]`.

### Best #3: state = (`BG`, `ACIL`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
O X O .
. . X .
O . . O
. . . .
```

- **Empirical** P(to-move wins) = **0.3906**  (n = 125,000, n_wins = 48,824)
- **7-feat** prediction = `+0.4220`  →  error = `-0.0314`
- **11-feat** prediction = `+0.4021`  →  error = `-0.0116`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 6 | 2 | 6 | 2 | 1 | 0 | 1 | 2 | 4 | 0 | 2 |

Mid-game state (6 tokens). Threat saturation: own matches (`m20 = 0`, `imm_win_to_move = 0`); opp matches (`m02 = 2`, `imm_win_opp = 2`). Both predictions in / in `[0, 1]`.

### Best #2: state = (`BEFL`, `GIO`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
. X . .
X X O .
O . . X
. . O .
```

- **Empirical** P(to-move wins) = **0.6099**  (n = 125,000, n_wins = 76,234)
- **7-feat** prediction = `+0.5960`  →  error = `+0.0139`
- **11-feat** prediction = `+0.5999`  →  error = `+0.0100`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 6 | 1 | 8 | 5 | 0 | 1 | 2 | 4 | 3 | 1 | 1 |

Mid-game state (7 tokens). Threat saturation: own matches (`m20 = 1`, `imm_win_to_move = 1`); opp matches (`m02 = 1`, `imm_win_opp = 1`). Both predictions in / in `[0, 1]`.

### Best #1: state = (`BFKL`, `AIJM`)

Board (`X` = to-move, `O` = opponent, `.` = empty):

```
O X . .
. X . .
O O X X
O . . .
```

- **Empirical** P(to-move wins) = **0.6075**  (n = 125,000, n_wins = 75,933)
- **7-feat** prediction = `+0.6000`  →  error = `+0.0075`
- **11-feat** prediction = `+0.6435`  →  error = `-0.0360`

| m01 | m02 | m10 | m11 | m12 | m20 | m21 | n_to_move | n_opp | imm_win_to_move | imm_win_opp |
|---|---|---|---|---|---|---|---|---|---|---|
| 3 | 3 | 8 | 3 | 1 | 2 | 3 | 4 | 4 | 2 | 2 |

Mid-game state (8 tokens). Threat saturation: own matches (`m20 = 2`, `imm_win_to_move = 2`); opp approximate (`m02 = 3`, `imm_win_opp = 2`). Both predictions in / in `[0, 1]`.

## 7. Acknowledgements

- **Charles Zheng** — designed the error-analysis protocol, specified the visualized + horizontal-feature-table format, and asked for failure-mode commentary derived from a top-20 scan.
- **Claude (Anthropic)** — implemented the report generator. The failure-mode bullets are computed from each row's feature values; no free-form generation.
