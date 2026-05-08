"""Find best- and worst-fit canonical states and re-evaluate them at high precision.

Workflow:
  1. Load merged_natural.tsv.
  2. Refit the weighted 7-feature OLS (no bootstrap needed; just point est).
  3. For each row, compute predicted = X * coef and empirical = n_wins / n_sims.
     Define residual = empirical - predicted.
  4. Sort by |residual| descending; select top 50 (worst) and bottom 50 (best).
  5. For each of these 100 states, run N_HIGH fresh sims to get a high-precision
     empirical estimate. N_HIGH chosen to fit the 10-min wall budget.
  6. Write a TSV in the existing merged-row format with two extra columns
     (predicted_win, empirical_win) where n_sims and n_wins are the high-res
     values. Order: 50 worst (worst first), then 50 best (best last).
"""
import argparse
import csv
import os
import time

import numpy as np

from sim import ML_FEATURE_NAMES, ml_features, simulate
import random


MATRIX_FEATURES = ML_FEATURE_NAMES[:7]
ALL_FEATURES = list(ML_FEATURE_NAMES)


def load_merged_natural(path):
    """Returns list of dicts: key_s1, key_s2, n_sims, n_wins, features (11-list)."""
    rows = []
    with open(path) as f:
        # Skip leading comment lines
        first = f.readline()
        while first.startswith("#"):
            first = f.readline()
        # `first` is now the header
        header = first.rstrip("\n").split("\t")
        reader = csv.reader(f, delimiter="\t")
        for r in reader:
            rec = dict(zip(header, r))
            rec["n_sims"] = int(rec["n_sims"])
            rec["n_wins"] = int(rec["n_wins"])
            features = [int(rec[name]) for name in ML_FEATURE_NAMES]
            rows.append({
                "key_s1": rec["key_s1"],
                "key_s2": rec["key_s2"],
                "n_sims": rec["n_sims"],
                "n_wins": rec["n_wins"],
                "features": features,
            })
    return rows


def fit_weighted_ols_for_features(rows, feature_names):
    """Fit weighted OLS on the given subset of features. Returns coef, X_aug, y, w."""
    name_to_idx = {n: i for i, n in enumerate(ML_FEATURE_NAMES)}
    cols = [name_to_idx[n] for n in feature_names]
    X = np.array([[r["features"][c] for c in cols] for r in rows], dtype=float)
    w = np.array([r["n_sims"] for r in rows], dtype=float)
    y_mean = np.array([r["n_wins"] / r["n_sims"] for r in rows], dtype=float)
    X_aug = np.column_stack([np.ones(len(X)), X])
    sw = np.sqrt(w)
    coef, *_ = np.linalg.lstsq(sw[:, None] * X_aug, sw * y_mean, rcond=None)
    return coef, X_aug, y_mean, w


def select_extremes(coef_7, X_aug_7, y_mean):
    """Selection is based on the 7-feature model's residuals."""
    predicted = X_aug_7 @ coef_7
    residual = y_mean - predicted
    abs_res = np.abs(residual)
    sorted_idx = np.argsort(-abs_res)  # descending
    worst_50 = sorted_idx[:50]
    best_50 = sorted_idx[-50:]   # already in descending |residual| (smallest at the very end)
    selected = np.concatenate([worst_50, best_50])
    return selected, predicted, residual


def high_res_sim(state_key, n_sims, rng):
    n_wins = 0
    for _ in range(n_sims):
        winner, _ = simulate(state_key, next_player=1, rng=rng)
        if winner == 1:
            n_wins += 1
    return n_wins


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--in-tsv", default="merged_natural.tsv")
    p.add_argument("--out-tsv", default="error_analysis_top100.tsv")
    p.add_argument("--n-high", type=int, default=125_000,
                   help="High-res sims per selected state (default 125K → ~10 min for 100 states)")
    p.add_argument("--seed", type=int, default=42)
    args = p.parse_args()

    print(f"Loading {args.in_tsv}...")
    rows = load_merged_natural(args.in_tsv)
    print(f"  {len(rows):,} canonical states loaded")

    print("Refitting weighted 7-feature OLS...")
    coef_7, X_aug_7, y_mean, w = fit_weighted_ols_for_features(rows, MATRIX_FEATURES)
    predicted_7 = X_aug_7 @ coef_7
    print(f"  coef_7 = {coef_7}")

    print("Refitting weighted 11-feature OLS...")
    coef_11, X_aug_11, _, _ = fit_weighted_ols_for_features(rows, ALL_FEATURES)
    predicted_11 = X_aug_11 @ coef_11
    print(f"  coef_11 = {coef_11}")

    # Selection is based on the 7-feature model's residuals.
    selected_idx, _, residual = select_extremes(coef_7, X_aug_7, y_mean)
    print(f"  selected {len(selected_idx)} states  "
          f"(|residual_7| range: {np.abs(residual[selected_idx]).max():.4f} → "
          f"{np.abs(residual[selected_idx]).min():.4f})")

    print(f"\nRunning high-resolution sims: n={args.n_high:,} per state, "
          f"100 states = {args.n_high * 100:,} total sims...")
    rng = random.Random(args.seed)
    high_results = []
    t0 = time.monotonic()
    for i, idx in enumerate(selected_idx):
        row = rows[idx]
        state_key = (row["key_s1"], row["key_s2"])
        n_wins_high = high_res_sim(state_key, args.n_high, rng)
        emp_high = n_wins_high / args.n_high
        pred_7 = float(predicted_7[idx])
        pred_11 = float(predicted_11[idx])
        high_results.append({
            "key_s1": row["key_s1"],
            "key_s2": row["key_s2"],
            "n_sims": args.n_high,
            "n_wins": n_wins_high,
            "features": row["features"],
            "predicted_win_7": pred_7,
            "predicted_win_11": pred_11,
            "empirical_win": emp_high,
        })
        if (i + 1) % 10 == 0 or i + 1 == len(selected_idx):
            elapsed = time.monotonic() - t0
            sims_done = (i + 1) * args.n_high
            rate = sims_done / elapsed if elapsed > 0 else 0
            print(f"  ...{i+1}/{len(selected_idx)}  "
                  f"sims={sims_done:,}  elapsed={elapsed:.1f}s  rate={rate/1000:.1f}K sims/s")

    print(f"\nWriting {args.out_tsv}...")
    header = (["key_s1", "key_s2", "n_sims", "n_wins"] +
              ML_FEATURE_NAMES +
              ["predicted_win_7", "predicted_win_11", "empirical_win"])
    with open(args.out_tsv, "w", newline="") as f:
        f.write(f"# n_high={args.n_high}  source={args.in_tsv}  seed={args.seed}\n")
        f.write("# selection by 7-feature residual: rows 1-50 worst-first, rows 51-100 best-last\n")
        wcsv = csv.writer(f, delimiter="\t")
        wcsv.writerow(header)
        for r in high_results:
            wcsv.writerow([
                r["key_s1"], r["key_s2"], r["n_sims"], r["n_wins"],
                *r["features"],
                f"{r['predicted_win_7']:.6f}",
                f"{r['predicted_win_11']:.6f}",
                f"{r['empirical_win']:.6f}",
            ])

    size = os.path.getsize(args.out_tsv)
    total_wall = time.monotonic() - t0
    print(f"  wrote {args.out_tsv} ({size:,} bytes = {size/1024:.0f} KB)")
    print(f"  high-res sim phase wall time: {total_wall:.1f}s = {total_wall/60:.2f} min")


if __name__ == "__main__":
    main()
