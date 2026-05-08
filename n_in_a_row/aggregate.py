"""Subsample, symmetry-merge, and re-simulate game-state datasets.

Workflow:
  1. Load raw `data.tsv` rows from `collect_data.py`.
  2. Subsample at the game level (keep p fraction of distinct game_ids).
  3. Compute the canonical (D4 + player-swap-via-perspective) form of each row.
  4. For each canonical state visited by the subsample, run `n` fresh
     simulations from the canonical representative (with to_move = 1)
     and store (count = n, n_wins).
  5. Write a merged TSV with one row per canonical state.

Includes a `synthetic_test()` that hard-checks the merge logic against
the orbit-stabilizer theorem before any real data is used.
"""
import argparse
import csv
import random
import time

from sim import (
    LETTERS,
    ML_FEATURE_NAMES,
    SYMMETRIES,
    apply_symmetry,
    canonical_form,
    line_features,
    ml_features,
    simulate,
)


def load_raw_tsv(path):
    """Yields (game_id, t, to_move, label, state). Doesn't materialize features."""
    with open(path) as f:
        reader = csv.reader(f, delimiter="\t")
        header = next(reader)
        # We need to know which two letter-strings are stored. The current
        # collect_data.py does NOT store the raw state — it stores only
        # features and to_move. So we cannot recover state from data.tsv.
        # The aggregation must collect raw states alongside.
        raise NotImplementedError(
            "data.tsv from collect_data.py does not include state strings; "
            "use collect_subsampled_states() to regenerate from games instead."
        )


def collect_subsampled_states(n_games, p, seed_games, max_turns=10_000):
    """Run n_games random games (deterministic with seed_games), then keep
    rows belonging to a random p-fraction of game_ids. Returns a list of
    (game_id, t, to_move, raw_state) tuples for the kept games' non-terminal
    states.

    This replaces the 'load data.tsv + subsample' path because the existing
    data.tsv lacks raw state strings.
    """
    rng = random.Random(seed_games)
    all_rows = []
    for game_id in range(n_games):
        winner, turns, history = simulate(
            ("", ""), next_player=1,
            max_turns=max_turns, rng=rng, record_history=True,
        )
        if winner is None:
            continue
        for t in range(turns):
            to_move = 1 if t % 2 == 0 else 2
            all_rows.append((game_id, t, to_move, history[t]))

    # Subsample at game level (deterministic by game_id with a fresh rng).
    sub_rng = random.Random(seed_games + 1)
    distinct_gids = sorted({r[0] for r in all_rows})
    keep = set(sub_rng.sample(distinct_gids, max(1, int(round(p * len(distinct_gids))))))
    return [r for r in all_rows if r[0] in keep]


def merge_by_canonical(rows):
    """rows: iterable of (state, to_move). Returns dict[key -> {rep, count}].

    The 'count' here counts how many input rows fell into each canonical bucket
    — used by the synthetic test. The downstream sim phase ignores it and
    runs `n` fresh sims per bucket.
    """
    merged = {}
    for state, to_move in rows:
        key = canonical_form(state, to_move)
        if key not in merged:
            merged[key] = {"rep": key, "count": 0}
        merged[key]["count"] += 1
    return merged


def run_sims_per_state(canonical_keys, n, seed):
    """For each canonical key (own_canon, opp_canon), simulate n random
    completions starting from that state with to_move = 1; count wins.

    Returns list of dicts with: key, n_wins, count=n, features (11-dim).
    """
    rng = random.Random(seed)
    out = []
    for key in canonical_keys:
        state = key  # canonical form is already a (s1, s2) tuple
        n_wins = 0
        for _ in range(n):
            winner, _ = simulate(state, next_player=1, rng=rng)
            if winner == 1:
                n_wins += 1
        feats = ml_features(state, 1)
        out.append({
            "key_s1": key[0],
            "key_s2": key[1],
            "count": n,
            "n_wins": n_wins,
            "features": feats,
        })
    return out


def write_merged_tsv(rows, path):
    header = ["key_s1", "key_s2", "count", "n_wins"] + ML_FEATURE_NAMES
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(header)
        for r in rows:
            w.writerow([r["key_s1"], r["key_s2"], r["count"], r["n_wins"], *r["features"]])


def synthetic_test():
    """Verify the canonical merge passes the orbit-stabilizer test.

    Three states with distinct line-feature matrices, expanded under all 8
    spatial D4 symmetries (with multiplicities), should fall into 3 buckets
    of count 8 each.
    """
    state_A = ("A", "")
    state_B = ("B", "")
    state_F = ("F", "")
    assert line_features(state_A, 1) != line_features(state_B, 1)
    assert line_features(state_A, 1) != line_features(state_F, 1)
    assert line_features(state_B, 1) != line_features(state_F, 1)

    inputs = []
    for st in (state_A, state_B, state_F):
        for perm in SYMMETRIES:
            inputs.append((apply_symmetry(st, perm), 1))
    assert len(inputs) == 24

    merged = merge_by_canonical(inputs)
    assert len(merged) == 3, f"expected 3 buckets, got {len(merged)}"
    counts = sorted(v["count"] for v in merged.values())
    assert counts == [8, 8, 8], f"expected counts [8,8,8], got {counts}"

    # Verify the canonical reps are the lex-min forms we expect:
    keys = set(merged.keys())
    assert keys == {("A", ""), ("B", ""), ("F", "")}, keys
    print("synthetic_test passed: 3 buckets x count 8")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--n-games", type=int, default=25600)
    p.add_argument("--p", type=float, required=True)
    p.add_argument("--n-sims", type=int, required=True)
    p.add_argument("--seed-games", type=int, default=0)
    p.add_argument("--seed-sims", type=int, default=1)
    p.add_argument("--out", required=True)
    args = p.parse_args()

    synthetic_test()

    t0 = time.monotonic()
    sub_rows = collect_subsampled_states(args.n_games, args.p, args.seed_games)
    t1 = time.monotonic()
    merged = merge_by_canonical([(state, to_move) for (_, _, to_move, state) in sub_rows])
    t2 = time.monotonic()
    sim_rows = run_sims_per_state(list(merged.keys()), args.n_sims, args.seed_sims)
    t3 = time.monotonic()
    write_merged_tsv(sim_rows, args.out)

    print(f"games:              {args.n_games}")
    print(f"subsample p:        {args.p}")
    print(f"sub rows:           {len(sub_rows)}")
    print(f"canonical states:   {len(merged)}")
    print(f"sims per state:     {args.n_sims}")
    print(f"total sims:         {len(merged) * args.n_sims}")
    print(f"games-collect time: {t1-t0:.1f}s")
    print(f"merge time:         {t2-t1:.2f}s")
    print(f"per-state sim time: {t3-t2:.1f}s")
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
