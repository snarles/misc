"""Walk an empty-board game forward and print each turn.

Usage:
    python3 play.py                     # 20 turns, no features
    python3 play.py --features          # with line-feature matrices
    python3 play.py --max-turns 30 --seed 0
"""
import argparse
import random

from sim import render_turn, simulate


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--max-turns", type=int, default=20)
    p.add_argument("--features", action="store_true")
    p.add_argument("--seed", type=int, default=None)
    args = p.parse_args()

    rng = random.Random(args.seed)
    winner, turns, history = simulate(
        ("", ""),
        next_player=1,
        max_turns=args.max_turns,
        rng=rng,
        record_history=True,
    )
    for t, state in enumerate(history):
        print(render_turn(state, t, show_features=args.features))
        print()
    if winner:
        print(f"Winner: P{winner} after {turns} turns")
    else:
        print(f"No winner within {turns} turns (cap)")


if __name__ == "__main__":
    main()
