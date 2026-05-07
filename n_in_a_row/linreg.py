"""OLS linear regression with block bootstrap CIs (resampling by game_id).

Provides three named specifications:
  - "p1_only_7":  to_move == 1 only, 7 matrix features
  - "both_7":     all rows, 7 matrix features
  - "both_11":    all rows, 11 features (matrix + token counts + immediate-win counts)

The first two together form the symmetry sanity check; the third is the
full-feature regression.
"""
import argparse
import csv

import numpy as np

from sim import ML_FEATURE_NAMES


MATRIX_FEATURES = ML_FEATURE_NAMES[:7]              # m01..m21
ALL_FEATURES = list(ML_FEATURE_NAMES)               # 11 features

SPECS = {
    "p1_only_7": {"feats": MATRIX_FEATURES, "to_move": {1}},
    "both_7":    {"feats": MATRIX_FEATURES, "to_move": {1, 2}},
    "both_11":   {"feats": ALL_FEATURES,    "to_move": {1, 2}},
}


def load_tsv(path):
    with open(path) as f:
        reader = csv.reader(f, delimiter="\t")
        header = next(reader)
        rows = [list(map(int, r)) for r in reader]
    arr = np.array(rows, dtype=np.int64)
    cols = {name: i for i, name in enumerate(header)}
    return arr, cols


def select(arr, cols, feature_names, to_move_filter):
    mask = np.isin(arr[:, cols["to_move"]], list(to_move_filter))
    sub = arr[mask]
    X = np.column_stack([sub[:, cols[name]] for name in feature_names]).astype(float)
    y = sub[:, cols["label"]].astype(float)
    gids = sub[:, cols["game_id"]]
    return X, y, gids


def fit_ols(X, y):
    X_aug = np.column_stack([np.ones(len(X)), X])
    coef, *_ = np.linalg.lstsq(X_aug, y, rcond=None)
    return coef


def block_bootstrap_ci(X, y, gids, n_boot=1000, ci=95, seed=0):
    rng = np.random.default_rng(seed)
    unique_games = np.unique(gids)
    by_game = {g: np.where(gids == g)[0] for g in unique_games}
    n_games = len(unique_games)
    n_features_with_intercept = X.shape[1] + 1
    coefs = np.empty((n_boot, n_features_with_intercept))
    for b in range(n_boot):
        sampled_games = rng.choice(unique_games, size=n_games, replace=True)
        idx_parts = [by_game[g] for g in sampled_games]
        idx = np.concatenate(idx_parts)
        coefs[b] = fit_ols(X[idx], y[idx])
    lo_p = (100 - ci) / 2
    hi_p = 100 - lo_p
    lo = np.percentile(coefs, lo_p, axis=0)
    hi = np.percentile(coefs, hi_p, axis=0)
    return lo, hi


def fit_and_bootstrap(arr, cols, feature_names, to_move_filter, n_boot=1000, seed=0):
    X, y, gids = select(arr, cols, feature_names, to_move_filter)
    coef = fit_ols(X, y)
    lo, hi = block_bootstrap_ci(X, y, gids, n_boot=n_boot, seed=seed)
    return {
        "names": ["(intercept)"] + list(feature_names),
        "coef": coef,
        "lo": lo,
        "hi": hi,
        "n_rows": len(y),
        "n_games": len(np.unique(gids)),
    }


def sign_judgement(lo, hi):
    if lo > 0:
        return "+"
    if hi < 0:
        return "-"
    return "?"


def n_sign_confident(result):
    return sum(
        1 for i in range(1, len(result["coef"]))
        if sign_judgement(result["lo"][i], result["hi"][i]) != "?"
    )


def print_table(name, result):
    print(f"=== {name}  (n_rows={result['n_rows']}, n_games={result['n_games']}) ===")
    print(f"{'feature':<20} {'coef':>10} {'lo':>10} {'hi':>10}  sign?")
    for i, fname in enumerate(result["names"]):
        c, lo, hi = result["coef"][i], result["lo"][i], result["hi"][i]
        sj = "" if i == 0 else sign_judgement(lo, hi)
        print(f"{fname:<20} {c:>10.4f} {lo:>10.4f} {hi:>10.4f}  {sj}")
    n_feats = len(result["coef"]) - 1
    print(f"  sign-confident: {n_sign_confident(result)}/{n_feats}")
    print()


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data", default="data.tsv")
    p.add_argument("--n-boot", type=int, default=1000)
    p.add_argument("--seed", type=int, default=0)
    args = p.parse_args()
    arr, cols = load_tsv(args.data)
    for name, spec in SPECS.items():
        result = fit_and_bootstrap(
            arr, cols, spec["feats"], spec["to_move"],
            n_boot=args.n_boot, seed=args.seed,
        )
        print_table(name, result)


if __name__ == "__main__":
    main()
