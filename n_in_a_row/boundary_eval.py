"""Passive boundary evaluation harness.

Takes a 4-axis boundary `(max_diff, max_tokens, race_a, race_b)`:
  inside iff
    |n_to_move - n_opp| <= max_diff
    n_to_move + n_opp  <= max_tokens
    NOT (imm_win_to_move > race_a AND imm_win_opp > race_b)

Steps:
  1. Filter merged_natural.tsv rows by the boundary.
  2. Refit weighted 7-feature OLS on the filtered rows.
  3. Rank inside-rows by natural residual; take top 20.
  4. Run n_high_res fresh sims per top-20 state.
  5. Compute high-res empirical and abs error vs prediction.
  6. Print a structured markdown report for Claude to reflect on.

This script makes NO decisions about expand/restrict. Decisions are made
by Claude (in persona) after reading the report.
"""
import argparse
import csv
import random
import time
from dataclasses import dataclass

import numpy as np

from sim import ML_FEATURE_NAMES, immediate_win_cells, simulate


MATRIX_FEATURES = ML_FEATURE_NAMES[:7]


@dataclass
class Boundary:
    max_diff: int
    max_tokens: int
    race_a: int
    race_b: int

    def __str__(self):
        return (f"(max_diff={self.max_diff}, max_tokens={self.max_tokens}, "
                f"race_a={self.race_a}, race_b={self.race_b})")


def feat_idx(name):
    return ML_FEATURE_NAMES.index(name)


def is_inside(feats, b: Boundary):
    n_tm = feats[feat_idx("n_to_move")]
    n_opp = feats[feat_idx("n_opp")]
    iw_tm = feats[feat_idx("imm_win_to_move")]
    iw_opp = feats[feat_idx("imm_win_opp")]
    if abs(n_tm - n_opp) > b.max_diff:
        return False
    if n_tm + n_opp > b.max_tokens:
        return False
    if iw_tm > b.race_a and iw_opp > b.race_b:
        return False
    return True


def load_merged_natural(path):
    rows = []
    header = None
    with open(path) as f:
        for line in f:
            if line.startswith("#"):
                continue
            if header is None:
                header = line.rstrip("\n").split("\t")
                continue
            cols = line.rstrip("\n").split("\t")
            rec = dict(zip(header, cols))
            features = [int(rec[name]) for name in ML_FEATURE_NAMES]
            rows.append({
                "key_s1": rec["key_s1"],
                "key_s2": rec["key_s2"],
                "n_sims": int(rec["n_sims"]),
                "n_wins": int(rec["n_wins"]),
                "features": features,
            })
    return rows


def fit_weighted_ols(X, y, w):
    X_aug = np.column_stack([np.ones(len(X)), X])
    sw = np.sqrt(w)
    coef, *_ = np.linalg.lstsq(sw[:, None] * X_aug, sw * y, rcond=None)
    return coef


def evaluate(merged_rows, boundary: Boundary, n_high_res=125_000, seed=0):
    inside = [r for r in merged_rows if is_inside(r["features"], boundary)]
    total_w_all = sum(r["n_sims"] for r in merged_rows)
    total_w_inside = sum(r["n_sims"] for r in inside)
    coverage = total_w_inside / total_w_all if total_w_all else 0.0
    state_frac = len(inside) / len(merged_rows) if merged_rows else 0.0

    if not inside:
        return {
            "boundary": boundary,
            "coverage": 0.0,
            "state_frac": 0.0,
            "n_inside": 0,
            "error": "boundary excludes all rows",
        }

    cols = [feat_idx(n) for n in MATRIX_FEATURES]
    X = np.array([[r["features"][c] for c in cols] for r in inside], dtype=float)
    w = np.array([r["n_sims"] for r in inside], dtype=float)
    y = np.array([r["n_wins"] / r["n_sims"] for r in inside], dtype=float)
    coef = fit_weighted_ols(X, y, w)
    X_aug = np.column_stack([np.ones(len(X)), X])
    pred = X_aug @ coef
    nat_resid = y - pred
    abs_nat = np.abs(nat_resid)
    order = np.argsort(-abs_nat)
    top20 = order[:min(20, len(inside))]

    rng = random.Random(seed)
    high_res = []
    for idx in top20:
        r = inside[idx]
        state = (r["key_s1"], r["key_s2"])
        n_wins = 0
        for _ in range(n_high_res):
            winner, _ = simulate(state, next_player=1, rng=rng)
            if winner == 1:
                n_wins += 1
        emp = n_wins / n_high_res
        p = float(pred[idx])
        high_res.append({
            "state": state,
            "features": r["features"],
            "n_natural_sims": r["n_sims"],
            "natural_emp": float(y[idx]),
            "prediction": p,
            "natural_resid": float(nat_resid[idx]),
            "high_res_emp": emp,
            "high_res_err": emp - p,
            "high_res_abs_err": abs(emp - p),
        })

    abs_errs = [h["high_res_abs_err"] for h in high_res]
    max_abs = max(abs_errs)
    mean_abs = sum(abs_errs) / len(abs_errs)

    n_tm_idx = feat_idx("n_to_move")
    n_opp_idx = feat_idx("n_opp")
    iw_tm_idx = feat_idx("imm_win_to_move")
    iw_opp_idx = feat_idx("imm_win_opp")
    profile = {
        "mean_diff": float(np.mean([abs(h["features"][n_tm_idx] - h["features"][n_opp_idx]) for h in high_res])),
        "max_diff": int(np.max([abs(h["features"][n_tm_idx] - h["features"][n_opp_idx]) for h in high_res])),
        "mean_tokens": float(np.mean([h["features"][n_tm_idx] + h["features"][n_opp_idx] for h in high_res])),
        "max_tokens": int(np.max([h["features"][n_tm_idx] + h["features"][n_opp_idx] for h in high_res])),
        "n_race": int(sum(1 for h in high_res if h["features"][iw_tm_idx] > 0 and h["features"][iw_opp_idx] > 0)),
        "n_imm_tm_only": int(sum(1 for h in high_res if h["features"][iw_tm_idx] > 0 and h["features"][iw_opp_idx] == 0)),
        "n_imm_opp_only": int(sum(1 for h in high_res if h["features"][iw_tm_idx] == 0 and h["features"][iw_opp_idx] > 0)),
        "n_no_imm": int(sum(1 for h in high_res if h["features"][iw_tm_idx] == 0 and h["features"][iw_opp_idx] == 0)),
    }

    return {
        "boundary": boundary,
        "n_inside": len(inside),
        "coverage": coverage,
        "state_frac": state_frac,
        "coef": coef,
        "high_res": high_res,
        "max_abs_err": max_abs,
        "mean_abs_err": mean_abs,
        "profile": profile,
    }


def render_report(result, n_high_res):
    if "error" in result:
        return f"## Boundary `{result['boundary']}`\n\n**ERROR**: {result['error']}\n"

    b = result["boundary"]
    out = []
    out.append(f"## Evaluation: {b}")
    out.append("")
    out.append(f"- **Inside rows**: {result['n_inside']:,}")
    out.append(f"- **Coverage** (n_sims-weighted): {result['coverage']:.4f}")
    out.append(f"- **State fraction** (unweighted): {result['state_frac']:.4f}")
    out.append(f"- **Max abs error** (top-20 high-res, n={n_high_res:,}): **{result['max_abs_err']:.4f}**")
    out.append(f"- **Mean abs error** (top-20): {result['mean_abs_err']:.4f}")
    out.append(f"- **Target**: max abs error < 0.08")
    out.append(f"- **Status**: {'PASS' if result['max_abs_err'] < 0.08 else 'FAIL'}")
    out.append("")

    out.append("### Refit 7-feature coefficients\n")
    out.append("| feature | coef |")
    out.append("|---|---|")
    out.append(f"| `(intercept)` | {result['coef'][0]:+.4f} |")
    for i, fname in enumerate(MATRIX_FEATURES):
        out.append(f"| `{fname}` | {result['coef'][i+1]:+.4f} |")
    out.append("")

    out.append("### Top-20 worst (by natural residual) — high-res evaluation\n")
    out.append("| # | state | n_tm | n_opp | iw_tm | iw_opp | natural_emp | pred | hi_res_emp | abs_err |")
    out.append("|---|---|---|---|---|---|---|---|---|---|")
    for i, h in enumerate(result["high_res"], 1):
        s1, s2 = h["state"]
        feats = h["features"]
        n_tm = feats[feat_idx("n_to_move")]
        n_opp = feats[feat_idx("n_opp")]
        iw_tm = feats[feat_idx("imm_win_to_move")]
        iw_opp = feats[feat_idx("imm_win_opp")]
        out.append(
            f"| {i} | (`{s1}`,`{s2}`) | {n_tm} | {n_opp} | {iw_tm} | {iw_opp} | "
            f"{h['natural_emp']:.3f} | {h['prediction']:+.3f} | {h['high_res_emp']:.3f} | "
            f"**{h['high_res_abs_err']:.4f}** |"
        )
    out.append("")

    p = result["profile"]
    out.append("### Top-20 violation profile\n")
    out.append(f"- mean |n_tm − n_opp| = **{p['mean_diff']:.2f}** (max {p['max_diff']})")
    out.append(f"- mean total tokens = **{p['mean_tokens']:.2f}** (max {p['max_tokens']})")
    out.append(f"- race condition (both have ≥ 1 imm-win): **{p['n_race']}/20**")
    out.append(f"- only to-move has imm-win: {p['n_imm_tm_only']}/20")
    out.append(f"- only opp has imm-win: {p['n_imm_opp_only']}/20")
    out.append(f"- no imm-wins: {p['n_no_imm']}/20")
    out.append("")
    return "\n".join(out)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--in-tsv", default="merged_natural.tsv")
    p.add_argument("--max-diff", type=int, required=True)
    p.add_argument("--max-tokens", type=int, required=True)
    p.add_argument("--race-a", type=int, required=True)
    p.add_argument("--race-b", type=int, required=True)
    p.add_argument("--n-high-res", type=int, default=125_000)
    p.add_argument("--seed", type=int, default=0)
    args = p.parse_args()

    rows = load_merged_natural(args.in_tsv)
    b = Boundary(args.max_diff, args.max_tokens, args.race_a, args.race_b)
    t0 = time.monotonic()
    result = evaluate(rows, b, args.n_high_res, args.seed)
    elapsed = time.monotonic() - t0
    print(render_report(result, args.n_high_res))
    print(f"\n_(eval time: {elapsed:.1f}s)_")


if __name__ == "__main__":
    main()
