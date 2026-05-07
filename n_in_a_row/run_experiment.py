"""Driver for the sanity-check experiment: collect, fit, iterate, write report.

Iterates N (number of games) by doubling, fits all three regression
specifications, stops when all three are sign-confident on enough features
(or hits the safety cap or wall budget), then writes report.md.
"""
import argparse
import time

from collect_data import collect, write_tsv
from linreg import (
    SPECS,
    fit_and_bootstrap,
    load_tsv,
    n_sign_confident,
    sign_judgement,
)


HYPOTHESES = {
    "m01":             ("-",   "mild – : latent opp presence"),
    "m02":             ("-",   "strong – : direct loss threat next turn"),
    "m10":             ("+",   "mild + : latent own presence"),
    "m11":             ("+",   "mild + : contested line, to-move has tempo advantage"),
    "m12":             ("-",   "– (less than m02) : one removal away from m02"),
    "m20":             ("+",   "strong + : direct win threat this turn"),
    "m21":             ("+",   "+ (less than m20) : one opp-removal away from m20"),
    "n_to_move":       ("+",   "+ : more own material → more potential"),
    "n_opp":           ("-",   "– : more opp material → opp closer to threats"),
    "imm_win_to_move": ("+",   "strong + : direct win this turn (collinear with m20)"),
    "imm_win_opp":     ("-",   "strong – : direct loss next turn (collinear with m02)"),
}


def run_iteration(n_games, seed, n_boot=1000):
    rows = collect(n_games, seed=seed)
    write_tsv(rows, "data.tsv")
    arr, cols = load_tsv("data.tsv")
    results = {}
    for name, spec in SPECS.items():
        results[name] = fit_and_bootstrap(
            arr, cols, spec["feats"], spec["to_move"],
            n_boot=n_boot, seed=seed,
        )
    return results


def fmt_coef(coef, lo, hi):
    return f"{coef:+.4f}  [{lo:+.4f}, {hi:+.4f}]"


def render_table(result, with_hypotheses=True):
    lines = []
    if with_hypotheses:
        lines.append(
            "| feature | coef | 95% CI | hypothesis | observed | agree? |"
        )
        lines.append("|---|---|---|---|---|---|")
    else:
        lines.append("| feature | coef | 95% CI |")
        lines.append("|---|---|---|")
    for i, name in enumerate(result["names"]):
        c, lo, hi = result["coef"][i], result["lo"][i], result["hi"][i]
        if i == 0:
            cells = [f"`{name}`", f"{c:+.4f}", f"[{lo:+.4f}, {hi:+.4f}]"]
            if with_hypotheses:
                cells += ["—", "—", "—"]
        else:
            obs = sign_judgement(lo, hi)
            if with_hypotheses:
                hyp_sign, hyp_text = HYPOTHESES.get(name, ("?", ""))
                if obs == "?":
                    agree = "?"
                else:
                    agree = "✓" if obs == hyp_sign else "✗"
                cells = [
                    f"`{name}`",
                    f"{c:+.4f}",
                    f"[{lo:+.4f}, {hi:+.4f}]",
                    hyp_sign,
                    obs,
                    agree,
                ]
            else:
                cells = [f"`{name}`", f"{c:+.4f}", f"[{lo:+.4f}, {hi:+.4f}]"]
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


def render_iteration_history(history):
    lines = ["| iter | N games | n rows (both) | n rows (P1-only) | sc p1_7 | sc both_7 | sc both_11 |",
             "|---|---|---|---|---|---|---|"]
    for i, h in enumerate(history):
        lines.append(
            f"| {i+1} | {h['n_games']} | {h['n_rows_both']} | {h['n_rows_p1']} | "
            f"{h['sc_p1_7']}/7 | {h['sc_both_7']}/7 | {h['sc_both_11']}/11 |"
        )
    return "\n".join(lines)


def render_symmetry_check(p1_result, both_result):
    p1_names = p1_result["names"][1:]
    pass_signs = []
    pass_overlap = []
    for i, name in enumerate(p1_names, start=1):
        p1_lo, p1_hi = p1_result["lo"][i], p1_result["hi"][i]
        bo_lo, bo_hi = both_result["lo"][i], both_result["hi"][i]
        p1_sign = sign_judgement(p1_lo, p1_hi)
        bo_sign = sign_judgement(bo_lo, bo_hi)
        signs_agree = p1_sign == "?" or bo_sign == "?" or p1_sign == bo_sign
        overlap = not (p1_hi < bo_lo or bo_hi < p1_lo)
        pass_signs.append((name, p1_sign, bo_sign, signs_agree))
        pass_overlap.append((name, overlap))
    sign_failures = [name for name, _, _, ok in pass_signs if not ok]
    overlap_failures = [name for name, ok in pass_overlap if not ok]
    lines = [
        "Comparing the **p1_only_7** and **both_7** regressions: if the perspective-flip in `ml_features` is correct, the matrix coefficients should agree in sign and have overlapping 95% CIs.",
        "",
        f"- **Sign agreement** (treating `?` as agreement, since wide CI ≠ disagreement): "
        f"{'PASS' if not sign_failures else 'FAIL — ' + ', '.join(sign_failures)}",
        f"- **CI overlap** (each feature's two CIs intersect): "
        f"{'PASS' if not overlap_failures else 'FAIL — ' + ', '.join(overlap_failures)}",
    ]
    return "\n".join(lines), bool(sign_failures or overlap_failures)


def write_report(history, results, path):
    p1_result = results["p1_only_7"]
    both_7 = results["both_7"]
    both_11 = results["both_11"]
    sym_text, sym_failed = render_symmetry_check(p1_result, both_7)

    lines = []
    lines.append("# Sanity check: 11-feature board state vs. win probability\n")

    lines.append("## 1. Purpose\n")
    lines.append(
        "This is an explicit **sanity check** on the 11-feature representation "
        "introduced for predicting win probability in the luck-only n-in-a-row "
        "simulation. The goal is **not** to ship a heuristic — it is to verify "
        "(a) that the perspective-flip used to canonicalize the to-move player "
        "is implemented correctly, and (b) that the coefficients have signs "
        "consistent with hand-derived priors. Any anomaly is more likely to "
        "indicate a bug or a flaw in the priors than a deep modeling insight at "
        "this stage.\n"
    )

    lines.append("## 2. Setup\n")
    lines.append(
        "- **Game**: 4×4 luck-only n-in-a-row (see `sim.py`). Each turn the active "
        "player draws a uniformly random cell of the 16; placing/removing/no-op "
        "depending on what's there. First to 3-in-a-line wins.\n"
        "- **Features (11)**: `ml_features(state, to_move)` returns 7 line-feature "
        "matrix entries (`m01, m02, m10, m11, m12, m20, m21`) plus token counts "
        "(`n_to_move, n_opp`) and immediate-win cell counts (`imm_win_to_move, "
        "imm_win_opp`), all from the to-move player's perspective.\n"
        "- **Data**: every non-terminal state from full random games starting "
        "from the empty board. Label is 1 iff the to-move player wins the game, "
        "else 0.\n"
        "- **Model**: OLS linear-probability model with intercept.\n"
        "- **Inference**: 95% confidence intervals via **block bootstrap** "
        "(resampling games with replacement, all rows from a sampled game kept "
        "together) — appropriate because rows from the same game are not "
        "independent.\n"
        "- **Specifications**:\n"
        "  1. `p1_only_7` — only `to_move == 1` rows, 7 matrix features. "
        "Avoids the perspective-flip code path entirely.\n"
        "  2. `both_7` — all rows, 7 matrix features. Uses the flip for "
        "P2-to-move rows.\n"
        "  3. `both_11` — all rows, all 11 features.\n"
    )

    lines.append("## 3. Hypothesized signs\n")
    lines.append(
        "Key insight informing the priors: filled \"dead\" lines are not really "
        "dead because of the removal mechanic. A `(2 own, 1 opp)` line is one "
        "opp-token-removal away from `(2, 0)` and so retains latent winning "
        "value; analogously `(1, 2)` retains latent loss-threat value. A `(1, 1)` "
        "line favors to-move because of tempo.\n"
    )
    lines.append("| feature | hypothesized sign | rationale |")
    lines.append("|---|---|---|")
    for name, (sign, text) in HYPOTHESES.items():
        lines.append(f"| `{name}` | {sign} | {text} |")
    lines.append("")
    lines.append(
        "Expected magnitude orderings: `|coef(m20)| > |coef(m21)|` and "
        "`|coef(m02)| > |coef(m12)|`.\n"
    )

    lines.append("## 4. Symmetry check\n")
    lines.append(sym_text + "\n")

    lines.append("## 5. Iteration history\n")
    lines.append(render_iteration_history(history) + "\n")

    lines.append("## 6. Final coefficient tables\n")
    lines.append("### 6.1 `p1_only_7` (matrix-only, P1-to-move only)\n")
    lines.append(render_table(p1_result) + "\n")
    lines.append("\n### 6.2 `both_7` (matrix-only, both to-move)\n")
    lines.append(render_table(both_7) + "\n")
    lines.append("\n### 6.3 `both_11` (full features, both to-move)\n")
    lines.append(render_table(both_11) + "\n")

    lines.append("## 7. Discussion\n")
    p1_sc = n_sign_confident(p1_result)
    b7_sc = n_sign_confident(both_7)
    b11_sc = n_sign_confident(both_11)
    lines.append(
        f"- After {history[-1]['n_games']} games "
        f"({history[-1]['n_rows_both']} state rows), sign-confident counts: "
        f"`p1_only_7` {p1_sc}/7, `both_7` {b7_sc}/7, `both_11` {b11_sc}/11.\n"
    )
    lines.append(
        "- **Symmetry**: " + (
            "the perspective-flip code path appears correct — `p1_only_7` and "
            "`both_7` agree on every matrix coefficient's sign and their 95% "
            "CIs overlap."
            if not sym_failed else
            "the symmetry sanity check FAILED. Investigate `ml_features` "
            "perspective handling before trusting any of the coefficient "
            "estimates."
        ) + "\n"
    )

    # Magnitude ordering checks
    def coef_for(result, name):
        idx = result["names"].index(name)
        return result["coef"][idx]

    m20_c = coef_for(both_7, "m20")
    m21_c = coef_for(both_7, "m21")
    m02_c = coef_for(both_7, "m02")
    m12_c = coef_for(both_7, "m12")
    lines.append(
        f"- **Magnitude orderings (point estimates, both_7)**: "
        f"`|m20|={abs(m20_c):.3f}` vs `|m21|={abs(m21_c):.3f}` "
        f"({'✓' if abs(m20_c) > abs(m21_c) else '✗'}); "
        f"`|m02|={abs(m02_c):.3f}` vs `|m12|={abs(m12_c):.3f}` "
        f"({'✓' if abs(m02_c) > abs(m12_c) else '✗'}).\n"
    )
    lines.append(
        "- Many features have CIs that span 0 even at the largest sample size — "
        "expected for the `m21`/`m12` pair (the latent-win/-loss effects are "
        "small) and for the `m20`/`imm_win_to_move` and `m02`/`imm_win_opp` "
        "collinear pairs in the 11-feature regression. OLS distributes the "
        "shared signal across each pair, widening both CIs.\n"
    )

    lines.append("## 8. Speculation\n")
    lines.append(
        "Possible improvements once the representation is trusted:\n"
        "- **Logistic regression** instead of OLS — proper for binary outcomes; "
        "won't change sign judgements much but will give better-calibrated "
        "probabilities.\n"
        "- **Drop one of each collinear pair** (`imm_win_*` or the matching "
        "`m02`/`m20`) — recover narrower CIs for the kept feature.\n"
        "- **Interaction terms**, especially `m20 × imm_win_opp` (race condition: "
        "I win this turn vs. lose next).\n"
        "- **Nonlinear models** (random forest, gradient boosting, MLP) — likely "
        "much better fits, since the win probability is plausibly nonlinear in "
        "feature counts (e.g. `imm_win_to_move ≥ 1` is essentially saturated for "
        "the immediate-win effect).\n"
        "- **Train on per-state Monte-Carlo win-probability estimates** rather "
        "than 0/1 game outcomes — lower-variance labels at the cost of needing "
        "many MC rollouts per training point.\n"
        "- **Add features** that the matrix can't see: # cells where opp removing "
        "one of our tokens drops us out of an `m20` line (\"vulnerable threats\"), "
        "or specifically structured threats like double-`m20` (fork) counts.\n"
    )

    lines.append("## 9. Acknowledgements\n")
    lines.append(
        "- **Charles Zheng** (human author) — designed the game, the feature "
        "representation, the symmetry analysis, and this experimental protocol; "
        "specified what \"sanity check\" should mean here, including the "
        "matrix-only-vs-full split and the P1-only symmetry comparison.\n"
        "- **Claude (Anthropic)** — implemented the simulation, feature "
        "extraction, data collection, regression with block bootstrap, and "
        "drafted this report under Charles's direction.\n"
    )

    with open(path, "w") as f:
        f.write("\n".join(lines))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--seed", type=int, default=0)
    p.add_argument("--start-n", type=int, default=100)
    p.add_argument("--max-n", type=int, default=102_400)
    p.add_argument("--threshold", type=int, default=9,
                   help="Min sign-confident count across all specs (out of their feature count) to stop")
    p.add_argument("--n-boot", type=int, default=1000)
    p.add_argument("--wall-budget-sec", type=float, default=3 * 3600)
    args = p.parse_args()

    N = args.start_n
    history = []
    start = time.monotonic()
    last_results = None
    while True:
        t0 = time.monotonic()
        results = run_iteration(N, seed=args.seed, n_boot=args.n_boot)
        elapsed = time.monotonic() - t0
        sc_p1_7 = n_sign_confident(results["p1_only_7"])
        sc_both_7 = n_sign_confident(results["both_7"])
        sc_both_11 = n_sign_confident(results["both_11"])
        history.append({
            "n_games": N,
            "n_rows_both": results["both_7"]["n_rows"],
            "n_rows_p1": results["p1_only_7"]["n_rows"],
            "sc_p1_7": sc_p1_7,
            "sc_both_7": sc_both_7,
            "sc_both_11": sc_both_11,
        })
        last_results = results
        print(
            f"[iter {len(history)}] N={N}  rows={results['both_7']['n_rows']}  "
            f"sc: p1_7={sc_p1_7}/7  both_7={sc_both_7}/7  both_11={sc_both_11}/11  "
            f"({elapsed:.1f}s)"
        )
        # Stopping: enough sign-confident across all specs, considering each spec's feature count
        # Use proportional threshold: at least (threshold/11) fraction in each spec
        thr_p1 = min(args.threshold, 7)        # 7 features in p1_only_7
        thr_b7 = min(args.threshold, 7)        # 7 features in both_7
        thr_b11 = args.threshold               # 11 features in both_11
        if sc_p1_7 >= thr_p1 and sc_both_7 >= thr_b7 and sc_both_11 >= thr_b11:
            print("Stopping: all specs sign-confident at threshold.")
            break
        if N * 2 > args.max_n:
            print(f"Stopping: would exceed max-n={args.max_n}.")
            break
        if time.monotonic() - start > args.wall_budget_sec:
            print(f"Stopping: wall budget exceeded.")
            break
        N *= 2

    write_report(history, last_results, "report.md")
    print(f"Wrote report.md ({len(history)} iterations).")


if __name__ == "__main__":
    main()
