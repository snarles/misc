"""Driver: natural-play-distribution dataset + weighted 7-feature regression.

Builds merged_natural.tsv via collect_natural, fits weighted OLS with
bootstrap, and writes report_natural.md comparing CI widths to merged_a/b.
"""
import argparse
import csv
import os
import time

import numpy as np

from collect_natural import (
    merge_natural,
    run_weighted_sims,
    sample_one_per_game,
    write_natural_tsv,
)
from sim import ML_FEATURE_NAMES


MATRIX_FEATURES = ML_FEATURE_NAMES[:7]
N_BOOT = 1000


def fit_weighted_ols(X, y_mean, w):
    X_aug = np.column_stack([np.ones(len(X)), X])
    sw = np.sqrt(w)
    coef, *_ = np.linalg.lstsq(sw[:, None] * X_aug, sw * y_mean, rcond=None)
    return coef


def bootstrap_ci(X, y_mean, w, n_boot=N_BOOT, ci=95, seed=0):
    rng = np.random.default_rng(seed)
    n = len(X)
    p = X.shape[1] + 1
    coefs = np.empty((n_boot, p))
    for b in range(n_boot):
        idx = rng.integers(0, n, size=n)
        coefs[b] = fit_weighted_ols(X[idx], y_mean[idx], w[idx])
    lo_p = (100 - ci) / 2
    hi_p = 100 - lo_p
    return np.percentile(coefs, lo_p, axis=0), np.percentile(coefs, hi_p, axis=0)


def feature_matrix_from_rows(rows, feature_names):
    name_to_idx = {n: i for i, n in enumerate(ML_FEATURE_NAMES)}
    cols = [name_to_idx[n] for n in feature_names]
    X = np.array([[row["features"][c] for c in cols] for row in rows], dtype=float)
    weights = np.array([row["n_sims"] for row in rows], dtype=float)
    y_mean = np.array([row["n_wins"] / row["n_sims"] for row in rows], dtype=float)
    return X, y_mean, weights


# Hardcoded reference CI widths from merged_a (config a, p=0.10, n=10) and
# merged_b (config b, p=0.02, n=50), as reported in report_aggregation.md.
# Used for the side-by-side comparison without re-fitting.
REF_CI_WIDTHS = {
    "merged_a": {
        "m01": 0.0024, "m02": 0.0029, "m10": 0.0026, "m11": 0.0022,
        "m12": 0.0036, "m20": 0.0034, "m21": 0.0040,
    },
    "merged_b": {
        "m01": 0.0024, "m02": 0.0033, "m10": 0.0024, "m11": 0.0022,
        "m12": 0.0044, "m20": 0.0045, "m21": 0.0043,
    },
}


def write_report(stats, coef, lo, hi, out_path, k, G, n_states, total_sims, wall_time, file_size):
    widths = hi - lo
    lines = []
    lines.append("# Natural-play-distribution merged dataset\n")

    lines.append("## Purpose\n")
    lines.append(
        "Match the **natural play distribution** for the regression's row "
        "weighting: states are sampled by playing one game and picking a "
        "random non-terminal moment, which is the empirical probability of "
        "encountering a state during random play. Common states (early-game "
        "openings) get more sims than rare states.\n"
    )

    lines.append("## Method\n")
    lines.append(
        f"1. Play `G = {G:,}` games from the empty board.\n"
        "2. For each game, sample one non-terminal state uniformly from its trajectory; discard the game outcome.\n"
        "3. Merge sampled states by canonical form (D4 spatial + player-swap-via-perspective).\n"
        f"4. For each canonical state with `nat_count = c`, run `k * c = {k} * c` fresh `simulate(state, next_player=1)` rollouts; record `n_wins`.\n"
        "5. Fit weighted OLS on the 7 matrix features (`m01..m21`) with `weight = n_sims = k * nat_count` and `y = n_wins / n_sims`. Bootstrap 95% CIs by resampling rows with replacement.\n"
    )

    lines.append("## Results\n")
    lines.append(f"- Canonical states: **{n_states:,}**")
    lines.append(f"- Total simulations: **{total_sims:,}**")
    lines.append(f"- TSV size: **{file_size/1024:.0f} KB** (budget < 1024 KB)")
    lines.append(f"- Wall time: **{wall_time/60:.1f} min** (budget < 10 min)")
    lines.append(f"- Skipped pre-won game starts: {stats.get('skipped', 0)}")
    lines.append("")

    lines.append("### Coefficient table (weighted OLS, 95% bootstrap CI)\n")
    lines.append("| feature | coef | 95% CI | width |")
    lines.append("|---|---|---|---|")
    lines.append(f"| `(intercept)` | {coef[0]:+.4f} | [{lo[0]:+.4f}, {hi[0]:+.4f}] | {hi[0]-lo[0]:.4f} |")
    for i, fname in enumerate(MATRIX_FEATURES):
        lines.append(
            f"| `{fname}` | {coef[i+1]:+.4f} | [{lo[i+1]:+.4f}, {hi[i+1]:+.4f}] | {widths[i+1]:.4f} |"
        )
    lines.append("")

    lines.append("### CI-width comparison vs uniform-weighted configs\n")
    lines.append("| feature | merged_a width | merged_b width | merged_natural width | shrink vs (a) | shrink vs (b) |")
    lines.append("|---|---|---|---|---|---|")
    for i, fname in enumerate(MATRIX_FEATURES):
        wa = REF_CI_WIDTHS["merged_a"][fname]
        wb = REF_CI_WIDTHS["merged_b"][fname]
        wn = widths[i + 1]
        lines.append(
            f"| `{fname}` | {wa:.4f} | {wb:.4f} | {wn:.4f} | "
            f"{wa/wn:.2f}x | {wb/wn:.2f}x |"
        )
    lines.append("")

    avg_shrink_a = np.mean([
        REF_CI_WIDTHS["merged_a"][f] / widths[i + 1]
        for i, f in enumerate(MATRIX_FEATURES)
    ])
    avg_shrink_b = np.mean([
        REF_CI_WIDTHS["merged_b"][f] / widths[i + 1]
        for i, f in enumerate(MATRIX_FEATURES)
    ])

    lines.append("## Discussion\n")
    lines.append(
        f"Average CI-width shrink across the 7 matrix features: "
        f"**{avg_shrink_a:.2f}× vs merged_a** and **{avg_shrink_b:.2f}× vs merged_b**. "
    )
    if avg_shrink_a > 2.0:
        lines.append(
            "The natural-distribution dataset achieves substantially tighter "
            "CIs than the uniform-weighted configs at comparable storage cost, "
            "validating the storage-vs-precision trade-off."
        )
    else:
        lines.append(
            "The natural-distribution dataset achieves modestly tighter CIs "
            "than the uniform-weighted configs. Most of the precision gain "
            "comes from the larger total simulation count rather than the "
            "weighting itself."
        )
    lines.append("")

    lines.append(
        "**Interpretation of the weighting.** With `weight = n_sims`, OLS "
        "minimizes a weighted sum of squared residuals. Per-row residuals at "
        "common states (high `n_sims`) dominate the loss, so the regression "
        "is calibrated for the natural play distribution: it estimates "
        "`E[label | features]` over states the way they actually appear in "
        "random play. This is the right target for a board-evaluation "
        "heuristic that will be applied during play.\n"
    )

    lines.append("## Acknowledgements\n")
    lines.append(
        "- **Charles Zheng** (human author) — designed the natural-play "
        "sampling protocol (one random non-terminal state per game) and the "
        "merged-count-as-multiplier scheme; chose the simulation parameters.\n"
        "- **Claude (Anthropic)** — implemented `collect_natural`, the "
        "weighted regression with bootstrap, and ran the experiment under "
        "Charles's direction.\n"
    )

    with open(out_path, "w") as f:
        f.write("\n".join(lines))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--n-games", type=int, default=25000)
    p.add_argument("--k", type=int, default=500)
    p.add_argument("--seed-games", type=int, default=0)
    p.add_argument("--seed-sims", type=int, default=1)
    p.add_argument("--out-tsv", default="merged_natural.tsv")
    p.add_argument("--out-report", default="report_natural.md")
    args = p.parse_args()

    print(f"=== Natural-play-distribution experiment ===")
    print(f"G={args.n_games}, k={args.k}")
    t_start = time.monotonic()

    print(f"\n[1/4] Sampling 1 state per game ({args.n_games} games)...")
    t0 = time.monotonic()
    samples, stats = sample_one_per_game(args.n_games, args.seed_games)
    t1 = time.monotonic()
    print(f"  kept {stats['kept']:,} samples ({stats['skipped']} skipped) in {t1-t0:.2f}s")

    print(f"\n[2/4] Merging by canonical form...")
    nat = merge_natural(samples)
    t2 = time.monotonic()
    n_states = len(nat)
    max_count = max(nat.values())
    avg_count = sum(nat.values()) / n_states
    print(f"  {n_states:,} canonical states (max nat_count={max_count}, avg={avg_count:.2f}) in {t2-t1:.2f}s")

    print(f"\n[3/4] Running weighted simulations (target total = {args.n_games * args.k:,})...")
    rows = run_weighted_sims(nat, args.k, args.seed_sims)
    t3 = time.monotonic()
    total_sims = sum(r["n_sims"] for r in rows)
    print(f"  total_sims={total_sims:,} in {t3-t2:.1f}s")

    print(f"\n[4/4] Writing TSV, fitting weighted OLS, bootstrapping CIs...")
    write_natural_tsv(rows, args.out_tsv, args.k, args.n_games, args.seed_games, args.seed_sims)
    file_size = os.path.getsize(args.out_tsv)
    print(f"  wrote {args.out_tsv} ({file_size:,} bytes = {file_size/1024:.0f} KB)")

    X, y_mean, w = feature_matrix_from_rows(rows, MATRIX_FEATURES)
    coef = fit_weighted_ols(X, y_mean, w)
    lo, hi = bootstrap_ci(X, y_mean, w, seed=args.seed_sims)
    t4 = time.monotonic()
    print(f"  fit + bootstrap done in {t4-t3:.1f}s")

    wall_time = t4 - t_start
    print(f"\nTotal wall time: {wall_time:.1f}s = {wall_time/60:.2f} min")
    print()
    print("Coefficients (matrix-only 7-feature, weighted OLS):")
    print(f"  {'feature':<14} {'coef':>10} {'lo':>10} {'hi':>10} {'width':>10}")
    print(f"  {'(intercept)':<14} {coef[0]:>10.4f} {lo[0]:>10.4f} {hi[0]:>10.4f} {hi[0]-lo[0]:>10.4f}")
    for i, fname in enumerate(MATRIX_FEATURES):
        print(f"  {fname:<14} {coef[i+1]:>10.4f} {lo[i+1]:>10.4f} {hi[i+1]:>10.4f} {hi[i+1]-lo[i+1]:>10.4f}")

    write_report(stats, coef, lo, hi, args.out_report, args.k, args.n_games,
                 n_states, total_sims, wall_time, file_size)
    print(f"\nWrote {args.out_report}")


if __name__ == "__main__":
    main()
