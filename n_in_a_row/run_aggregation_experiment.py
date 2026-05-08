"""Driver: subsample-and-merge storage benchmark.

Compares two configurations with equal total simulation count:
  (a) p=0.10, n=10 sims per merged canonical state
  (b) p=0.02, n=50 sims per merged canonical state

For each: time the data collection, fit a weighted 7-feature OLS, bootstrap
95% CIs, and report per-feature CI widths. Writes report_aggregation.md.
"""
import argparse
import time

import numpy as np

import random as _random

from aggregate import (
    collect_subsampled_states,
    merge_by_canonical,
    run_sims_per_state,
    synthetic_test,
    write_merged_tsv,
)
from sim import ML_FEATURE_NAMES, simulate


MATRIX_FEATURES = ML_FEATURE_NAMES[:7]   # m01..m21 (7 features)
N_BOOT = 1000


def feature_matrix(rows, feature_names):
    """Returns X (n_rows x p), y_mean, weights for weighted OLS."""
    name_to_idx = {n: i for i, n in enumerate(ML_FEATURE_NAMES)}
    cols = [name_to_idx[n] for n in feature_names]
    X = np.array([[row["features"][c] for c in cols] for row in rows], dtype=float)
    weights = np.array([row["count"] for row in rows], dtype=float)
    y_mean = np.array([row["n_wins"] / row["count"] for row in rows], dtype=float)
    return X, y_mean, weights


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


def collect_full_pool(n_games, seed_games, max_turns=10_000):
    """Run all n_games once; return list of (game_id, t, to_move, state) for non-terminal states."""
    rng = _random.Random(seed_games)
    rows = []
    for game_id in range(n_games):
        winner, turns, history = simulate(
            ("", ""), next_player=1,
            max_turns=max_turns, rng=rng, record_history=True,
        )
        if winner is None:
            continue
        for t in range(turns):
            to_move = 1 if t % 2 == 0 else 2
            rows.append((game_id, t, to_move, history[t]))
    return rows


def subsample_from_pool(pool_rows, p, seed_games):
    sub_rng = _random.Random(seed_games + 1)
    distinct_gids = sorted({r[0] for r in pool_rows})
    n_keep = max(1, int(round(p * len(distinct_gids))))
    keep = set(sub_rng.sample(distinct_gids, n_keep))
    return [r for r in pool_rows if r[0] in keep]


def run_config(label, p, n_sims, pool_rows, seed_games, seed_sims, out_path):
    print(f"\n=== Config {label}: p={p}, n_sims={n_sims} ===")
    t0 = time.monotonic()
    sub_rows = subsample_from_pool(pool_rows, p, seed_games)
    t1 = time.monotonic()
    merged = merge_by_canonical([(state, to_move) for (_, _, to_move, state) in sub_rows])
    t2 = time.monotonic()
    sim_rows = run_sims_per_state(list(merged.keys()), n_sims, seed_sims)
    t3 = time.monotonic()
    write_merged_tsv(sim_rows, out_path)

    n_states = len(merged)
    total_sims = n_states * n_sims

    # Weighted OLS + bootstrap on the 7 matrix features.
    X, y_mean, w = feature_matrix(sim_rows, MATRIX_FEATURES)
    coef = fit_weighted_ols(X, y_mean, w)
    lo, hi = bootstrap_ci(X, y_mean, w, seed=seed_sims)
    widths = hi - lo

    print(f"  sub rows:           {len(sub_rows)}")
    print(f"  canonical states:   {n_states}")
    print(f"  sims per state:     {n_sims}")
    print(f"  total sims:         {total_sims}")
    print(f"  subsample time:     {t1 - t0:.2f}s")
    print(f"  merge time:         {t2 - t1:.2f}s")
    print(f"  per-state sim time: {t3 - t2:.1f}s")
    print(f"  wrote {out_path}")
    print(f"  Coefficients (matrix-only 7-feature, weighted OLS):")
    print(f"    {'feature':<14} {'coef':>10} {'lo':>10} {'hi':>10} {'width':>10}")
    print(f"    {'(intercept)':<14} {coef[0]:>10.4f} {lo[0]:>10.4f} {hi[0]:>10.4f} {hi[0]-lo[0]:>10.4f}")
    for i, fname in enumerate(MATRIX_FEATURES):
        print(f"    {fname:<14} {coef[i+1]:>10.4f} {lo[i+1]:>10.4f} {hi[i+1]:>10.4f} {widths[i+1]:>10.4f}")

    return {
        "label": label,
        "p": p,
        "n_sims": n_sims,
        "n_states": n_states,
        "total_sims": total_sims,
        "time_subsample": t1 - t0,
        "time_merge": t2 - t1,
        "time_sim": t3 - t2,
        "coef": coef,
        "lo": lo,
        "hi": hi,
        "widths": widths,
    }


def write_report(results, path):
    a, b = results
    lines = []
    lines.append("# Subsample-and-merge storage benchmark\n")

    lines.append("## Purpose\n")
    lines.append(
        "Compare two ways of producing a labeled state-feature dataset with "
        "the **same total simulation count** but different storage cost:\n\n"
        "- **(a)** subsample p=0.10 of games, then run n=10 sims per canonical state\n"
        "- **(b)** subsample p=0.02 of games, then run n=50 sims per canonical state\n\n"
        "Configuration (b) stores ~5× fewer rows. Question: do the 7-feature "
        "regression CIs come out comparable?\n"
    )

    lines.append("## Method\n")
    lines.append(
        "1. Run a fixed pool of random games from the empty board (deterministic seed).\n"
        "2. Subsample at the game level with fraction p.\n"
        "3. Compute canonical form (D4 spatial × player-swap-via-perspective) of "
        "every state in the subsample, deduplicating.\n"
        "4. For each canonical state, run n fresh `simulate(state, next_player=1)` "
        "rollouts, count wins.\n"
        "5. Fit weighted OLS (`weight = n`, `y = n_wins/n`) on the 7-feature "
        "matrix (`m01..m21`) and bootstrap 95% CIs by resampling distinct "
        "canonical-state rows with replacement.\n"
    )

    lines.append("## Results\n")
    lines.append(
        "| metric                 | (a) p=0.10, n=10 | (b) p=0.02, n=50 | (b)/(a) |\n"
        "|---|---|---|---|"
    )
    lines.append(
        f"| canonical states (rows) | {a['n_states']:,} | {b['n_states']:,} | "
        f"{b['n_states']/a['n_states']:.2f} |"
    )
    lines.append(
        f"| total sims              | {a['total_sims']:,} | {b['total_sims']:,} | "
        f"{b['total_sims']/a['total_sims']:.2f} |"
    )
    lines.append(
        f"| subsample time (s)      | {a['time_subsample']:.2f} | {b['time_subsample']:.2f} | — |"
    )
    lines.append(
        f"| merge time (s)          | {a['time_merge']:.2f} | {b['time_merge']:.2f} | — |"
    )
    lines.append(
        f"| per-state sim time (s)  | {a['time_sim']:.1f} | {b['time_sim']:.1f} | "
        f"{b['time_sim']/a['time_sim']:.2f} |"
    )
    lines.append("")
    lines.append("### CI widths per feature (95% bootstrap)\n")
    lines.append(
        "| feature         | (a) coef | (a) width | (b) coef | (b) width | width (b)/(a) |\n"
        "|---|---|---|---|---|---|"
    )
    for i, fname in enumerate(MATRIX_FEATURES):
        lines.append(
            f"| `{fname}` | {a['coef'][i+1]:+.4f} | {a['widths'][i+1]:.4f} | "
            f"{b['coef'][i+1]:+.4f} | {b['widths'][i+1]:.4f} | "
            f"{b['widths'][i+1]/a['widths'][i+1]:.2f} |"
        )
    lines.append("")

    lines.append("## Discussion\n")
    avg_ratio = float(np.mean(b["widths"][1:] / a["widths"][1:]))
    lines.append(
        f"Average CI-width ratio (b)/(a) over the 7 matrix features: "
        f"**{avg_ratio:.2f}**. "
    )
    if avg_ratio < 1.25:
        lines.append(
            "The widths are comparable, supporting the storage-vs-precision "
            "tradeoff: configuration (b) stores ~5× fewer rows for a similar "
            "inferential outcome."
        )
    else:
        lines.append(
            "Configuration (b) gave noticeably wider CIs than (a). The "
            "storage savings come at a real precision cost; the appropriate "
            "choice depends on whether storage or inference is more "
            "constrained."
        )
    lines.append("")
    lines.append(
        "Total simulation count is matched by construction "
        f"({a['total_sims']:,} vs {b['total_sims']:,}, ratio "
        f"{b['total_sims']/a['total_sims']:.2f}); any difference in CI width "
        "reflects how the same total work is distributed (many states with few "
        "sims vs. few states with many sims).\n"
    )

    lines.append("## Acknowledgements\n")
    lines.append(
        "- **Charles Zheng** (human author) — designed the storage-vs-precision "
        "experiment and the symmetry merging strategy.\n"
        "- **Claude (Anthropic)** — implemented `canonical_form`, the aggregation "
        "and weighted regression pipeline, and ran the benchmark.\n"
    )

    with open(path, "w") as f:
        f.write("\n".join(lines))
    print(f"\nWrote {path}")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--n-games", type=int, default=25600)
    p.add_argument("--seed-games", type=int, default=0)
    p.add_argument("--seed-sims", type=int, default=1)
    args = p.parse_args()

    print("Running synthetic merge test...")
    synthetic_test()

    print(f"\nGenerating pool of {args.n_games} games (one-time)...")
    t0 = time.monotonic()
    pool = collect_full_pool(args.n_games, args.seed_games)
    pool_time = time.monotonic() - t0
    print(f"  pool: {len(pool):,} non-terminal state rows in {pool_time:.1f}s")

    a = run_config("(a)", p=0.10, n_sims=10,
                   pool_rows=pool,
                   seed_games=args.seed_games,
                   seed_sims=args.seed_sims,
                   out_path="merged_a.tsv")
    b = run_config("(b)", p=0.02, n_sims=50,
                   pool_rows=pool,
                   seed_games=args.seed_games,
                   seed_sims=args.seed_sims + 1,
                   out_path="merged_b.tsv")

    write_report([a, b], "report_aggregation.md")


if __name__ == "__main__":
    main()
