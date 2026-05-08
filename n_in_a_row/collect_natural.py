"""Collect a natural-play-distribution-weighted dataset.

Sampling design (per the user):
  1. Play G games from the empty board.
  2. For each game, sample 1 non-terminal state uniformly from its trajectory.
  3. Discard the game's outcome.
  4. Merge sampled states by canonical form (D4 + player-swap-via-perspective).
  5. For each canonical state with nat_count = c, run k * c fresh simulations
     and record n_wins.
  6. Write TSV with `key_s1, key_s2, n_sims, n_wins, m01..m21` and a comment
     header recording k, G, seeds. (nat_count is recoverable as n_sims / k.)

The downstream regression weights each row by `n_sims`, which is exactly the
natural-play-distribution weighting (since n_sims = k * nat_count).
"""
import argparse
import csv
import random
import time

from sim import (
    ML_FEATURE_NAMES,
    canonical_form,
    ml_features,
    simulate,
)


def sample_one_per_game(G, seed_games, max_turns=10_000):
    """Play G games. For each, pick one non-terminal state uniformly from its
    trajectory. Returns list of (state, to_move) and a dict of stats.
    """
    rng = random.Random(seed_games)
    samples = []
    skipped = 0
    for _ in range(G):
        winner, turns, history = simulate(
            ("", ""), next_player=1,
            max_turns=max_turns, rng=rng, record_history=True,
        )
        if winner is None or turns == 0:
            skipped += 1
            continue
        t = rng.randrange(turns)
        to_move = 1 if t % 2 == 0 else 2
        samples.append((history[t], to_move))
    return samples, {"skipped": skipped, "kept": len(samples)}


def merge_natural(samples):
    """Returns dict[canonical_key -> nat_count]."""
    nat = {}
    for state, to_move in samples:
        key = canonical_form(state, to_move)
        nat[key] = nat.get(key, 0) + 1
    return nat


def run_weighted_sims(nat_counts, k, seed_sims, progress_every=2000):
    """For each canonical key with nat_count c, run k * c fresh sims from
    (key, next_player=1). Returns list of dicts.
    """
    rng = random.Random(seed_sims)
    rows = []
    items = list(nat_counts.items())
    n_items = len(items)
    t0 = time.monotonic()
    sims_done = 0
    for i, (key, c) in enumerate(items):
        n_sims = k * c
        n_wins = 0
        for _ in range(n_sims):
            winner, _ = simulate(key, next_player=1, rng=rng)
            if winner == 1:
                n_wins += 1
        feats = ml_features(key, 1)
        rows.append({
            "key_s1": key[0],
            "key_s2": key[1],
            "n_sims": n_sims,
            "n_wins": n_wins,
            "features": feats,
            "nat_count": c,
        })
        sims_done += n_sims
        if (i + 1) % progress_every == 0 or i + 1 == n_items:
            elapsed = time.monotonic() - t0
            rate = sims_done / elapsed if elapsed > 0 else 0
            print(f"  ...row {i+1}/{n_items}  sims={sims_done:,}  "
                  f"elapsed={elapsed:.1f}s  rate={rate/1000:.1f}K sims/s")
    return rows


def write_natural_tsv(rows, path, k, G, seed_games, seed_sims):
    header = ["key_s1", "key_s2", "n_sims", "n_wins"] + ML_FEATURE_NAMES
    with open(path, "w", newline="") as f:
        f.write(f"# k={k}  G={G}  seed_games={seed_games}  seed_sims={seed_sims}\n")
        w = csv.writer(f, delimiter="\t")
        w.writerow(header)
        for r in rows:
            w.writerow([r["key_s1"], r["key_s2"], r["n_sims"], r["n_wins"], *r["features"]])


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--n-games", type=int, default=25000)
    p.add_argument("--k", type=int, default=600)
    p.add_argument("--seed-games", type=int, default=0)
    p.add_argument("--seed-sims", type=int, default=1)
    p.add_argument("--out", default="merged_natural.tsv")
    args = p.parse_args()

    print(f"Sampling 1 state per game from G={args.n_games} games (seed={args.seed_games})...")
    t0 = time.monotonic()
    samples, stats = sample_one_per_game(args.n_games, args.seed_games)
    t1 = time.monotonic()
    print(f"  kept {stats['kept']} samples ({stats['skipped']} skipped)  in {t1-t0:.2f}s")

    nat = merge_natural(samples)
    t2 = time.monotonic()
    print(f"Merged into {len(nat)} canonical states (max nat_count = {max(nat.values())})  in {t2-t1:.2f}s")

    print(f"Running k={args.k} * nat_count sims per canonical state "
          f"(target total sims = {args.n_games * args.k:,})...")
    rows = run_weighted_sims(nat, args.k, args.seed_sims)
    t3 = time.monotonic()
    total_sims = sum(r["n_sims"] for r in rows)
    print(f"  total sims executed: {total_sims:,}  in {t3-t2:.1f}s")

    write_natural_tsv(rows, args.out, args.k, args.n_games, args.seed_games, args.seed_sims)
    t4 = time.monotonic()

    import os
    size = os.path.getsize(args.out)
    print(f"Wrote {args.out} ({size:,} bytes = {size/1024:.0f} KB)")
    print(f"Total wall time: {t4-t0:.1f}s")


if __name__ == "__main__":
    main()
