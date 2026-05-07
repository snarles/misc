"""Collect labeled state-feature rows from random games starting from the empty board.

Each row is one non-terminal state from one game; label is 1 iff the to-move
player at that state ends up winning the game.

Output is a TSV with columns:
    game_id  t  to_move  label  m01 m02 m10 m11 m12 m20 m21
                                 n_to_move n_opp imm_win_to_move imm_win_opp
"""
import argparse
import csv
import random

from sim import ML_FEATURE_NAMES, ml_features, simulate


def collect(n_games, seed=None, max_turns=10_000):
    rng = random.Random(seed)
    rows = []
    for game_id in range(n_games):
        winner, turns, history = simulate(
            ("", ""), next_player=1,
            max_turns=max_turns, rng=rng, record_history=True,
        )
        if winner is None:
            continue
        for t in range(turns):
            state = history[t]
            to_move = 1 if t % 2 == 0 else 2
            label = 1 if winner == to_move else 0
            feats = ml_features(state, to_move)
            rows.append([game_id, t, to_move, label, *feats])
    return rows


def write_tsv(rows, path):
    header = ["game_id", "t", "to_move", "label"] + ML_FEATURE_NAMES
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(header)
        w.writerows(rows)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--n-games", type=int, default=100)
    p.add_argument("--out", default="data.tsv")
    p.add_argument("--seed", type=int, default=None)
    args = p.parse_args()
    rows = collect(args.n_games, seed=args.seed)
    write_tsv(rows, args.out)
    print(f"Wrote {len(rows)} rows from {args.n_games} games to {args.out}")


if __name__ == "__main__":
    main()
