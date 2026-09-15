"""Task 6: the strongest 2-fortress - two codons sharing no character.

A totalizer is a codon beating both members. Part 4 found that no pair inside any
3-fortress concedes fewer than 500 totalizers, which left open whether that is
the disjointness constraint biting or simply that the 3-fortress search never
optimised for pairs. This searches for pairs directly.

Two searches. ``--seeds`` samples random disjoint pairs and hill-climbs each,
mirroring analysis/fortress.py so the two are comparable. ``--exhaustive`` sweeps
every pair up to the simultaneous-permutation symmetry and returns the provable
optimum, which also scores how far the sampled search falls short.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/pair.py [--exhaustive]
"""

import argparse
import csv
import sys
import time

import numpy as np

from flies_and_goos.duo import BeatMatrixMissing, load_beat_matrix
from flies_and_goos.engine import N_CODONS, codon_index, codon_text, outcomes_against_all
from flies_and_goos.tables import ALPHABET

PAIR_SIZE = 2

#: Best partner for BFN from part 3. A regression check on the pair counter
#: only: BFN and BPN share B and N, so it is not a legal 2-fortress, and part 3
#: states it upper-bounds the global optimum without attaining it.
BFN_BPN_TOTALIZERS = 2041

#: Best pair found incidentally inside a 3-fortress (part 4). The disjoint
#: optimum is therefore at most this.
KNOWN_LEGAL_BAR = 1476

VERIFY_PROBES = 20
N_CHARS = len(ALPHABET)


def char_masks() -> np.ndarray:
    """36-bit mask of the distinct characters each codon uses, by codon index."""
    masks = np.zeros(N_CODONS, dtype=np.int64)
    positions = np.arange(N_CODONS)
    for divisor in (N_CHARS**2, N_CHARS, 1):
        chars = (positions // divisor) % N_CHARS
        masks |= np.int64(1) << chars.astype(np.int64)
    return masks


def sorted_codons() -> np.ndarray:
    """Indices of codons whose characters are non-decreasing.

    Every pair is equivalent under simultaneous permutation to one whose first
    member is sorted, so sweeping these as ``a`` covers every orbit.
    """
    positions = np.arange(N_CODONS)
    c0 = positions // N_CHARS**2
    c1 = (positions // N_CHARS) % N_CHARS
    c2 = positions % N_CHARS
    return np.flatnonzero((c0 <= c1) & (c1 <= c2))


def pair_counts(words: np.ndarray, row: np.ndarray) -> np.ndarray:
    """Totalizers of ``(row, b)`` for every codon b, in one pass.

    ``words`` is the beat-matrix viewed as uint64; np.bitwise_count works on
    729 words per row instead of 5,832 byte lookups.
    """
    return np.bitwise_count(words & row).sum(axis=1, dtype=np.int32)


def totalizers(words: np.ndarray, a: int, b: int) -> int:
    """Codons defeating both members. Row j lists who beats j, so AND them."""
    return int(np.bitwise_count(words[a] & words[b]).sum())


def totalizers_by_engine(a: str, b: str) -> int:
    """Independent recount by replaying matchups rather than reading the matrix."""
    return int(np.count_nonzero(
        (outcomes_against_all(a) < 0) & (outcomes_against_all(b) < 0)
    ))


def sample_pair(rng, masks):
    first = int(rng.integers(N_CODONS))
    pool = np.flatnonzero((masks & masks[first]) == 0)
    return [first, int(rng.choice(pool))]


def climb(words, masks, members, max_steps):
    """Best single-codon replacement until none improves."""
    members = list(members)
    trail = [totalizers(words, *members)]
    for _ in range(max_steps):
        best = None
        for slot in range(PAIR_SIZE):
            other = members[1 - slot]
            counts = pair_counts(words, words[other])
            counts[(masks & masks[other]) != 0] = np.iinfo(np.int32).max
            pick = int(np.lexsort((np.arange(N_CODONS), counts))[0])
            score = int(counts[pick])
            if score < trail[-1] and (best is None or score < best[2]):
                best = (slot, pick, score)
        if best is None:
            return members, trail, True
        slot, codon, score = best
        members[slot] = codon
        assert not (masks[members[0]] & masks[members[1]]), "swap broke disjointness"
        trail.append(score)
    return members, trail, False


def exhaustive(words, masks, top, report_every=500):
    """Sweep every pair up to the permutation symmetry. Returns (legal, any)."""
    firsts = sorted_codons()
    best_legal, best_any = [], []
    started = time.monotonic()
    for k, a in enumerate(firsts):
        counts = pair_counts(words, words[a])
        counts[a] = np.iinfo(np.int32).max  # a codon draws itself

        free = int(counts.argmin())
        best_any.append((int(counts[free]), int(a), free))

        legal = np.flatnonzero((masks & masks[a]) == 0)
        if legal.size:
            pick = legal[int(counts[legal].argmin())]
            best_legal.append((int(counts[pick]), int(a), int(pick)))

        if report_every and (k + 1) % report_every == 0:
            elapsed = time.monotonic() - started
            rate = (k + 1) / elapsed
            print(f"  {k + 1:>5}/{len(firsts)} first-members "
                  f"({elapsed:.0f}s elapsed, {(len(firsts) - k - 1) / rate:.0f}s left)"
                  f"  best legal so far {min(best_legal)[0]}", flush=True)

    best_legal.sort()
    best_any.sort()
    return best_legal[:top], best_any[:top]


def show(title, rows):
    print(f"\n{title}")
    print(f"  {'pair':12} {'totalizers':>11} {'fraction':>9}")
    for count, a, b in rows:
        print(f"  {codon_text(a)} {codon_text(b)}   {count:>11} "
              f"{count / N_CODONS:>9.4f}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seeds", type=int, default=200)
    parser.add_argument("--rng-seed", type=int, default=20260915)
    parser.add_argument("--max-steps", type=int, default=50)
    parser.add_argument("--out", default="pairs.csv")
    parser.add_argument("--top", type=int, default=12)
    parser.add_argument("--exhaustive", action="store_true")
    parser.add_argument("--time-slice", type=int, default=0,
                        help="time this many sweep scans, extrapolate, and stop")
    args = parser.parse_args()

    try:
        matrix = np.array(load_beat_matrix())
    except BeatMatrixMissing as error:
        sys.exit(str(error))
    words = matrix.view(np.uint64)
    print(f"beat matrix in memory ({matrix.nbytes / 1e6:.0f} MB, "
          f"{words.shape[1]} uint64 words per row)")

    masks = char_masks()

    # --- verification -----------------------------------------------------
    assert totalizers(words, codon_index("BFN"), codon_index("BPN")) == BFN_BPN_TOTALIZERS, (
        "pair counter disagrees with the published {BFN, BPN} figure"
    )
    rng = np.random.default_rng(args.rng_seed)
    for _ in range(3):
        a, b = sample_pair(rng, masks)
        assert totalizers(words, a, b) == totalizers_by_engine(
            codon_text(a), codon_text(b)
        ), f"matrix and engine disagree on {codon_text(a)} {codon_text(b)}"
    # The sweep's 5.5x reduction rests entirely on this symmetry.
    for _ in range(5):
        a, b = sample_pair(rng, masks)
        ta, tb = codon_text(a), codon_text(b)
        for order in ((1, 0, 2), (2, 1, 0), (1, 2, 0)):
            sa = codon_index("".join(ta[i] for i in order))
            sb = codon_index("".join(tb[i] for i in order))
            assert totalizers(words, sa, sb) == totalizers(words, a, b), (
                f"simultaneous permutation changed the count for {ta} {tb}"
            )
    print(
        f"verified: {{BFN, BPN}} reproduces {BFN_BPN_TOTALIZERS} (not disjoint, "
        f"and not the optimum - a regression check only); matrix agrees with the "
        f"engine; simultaneous permutation leaves counts unchanged"
    )

    if args.time_slice:
        firsts = sorted_codons()[: args.time_slice]
        started = time.monotonic()
        for a in firsts:
            pair_counts(words, words[a])
        per = (time.monotonic() - started) / len(firsts)
        total = per * len(sorted_codons())
        print(f"\n{len(firsts)} scans at {per * 1000:.0f} ms each -> full sweep "
              f"of {len(sorted_codons())} projects to {total / 60:.1f} min")
        return

    # --- sampled search ---------------------------------------------------
    results = []
    started = time.monotonic()
    for run in range(args.seeds):
        seed_members = sample_pair(rng, masks)
        members, trail, converged = climb(words, masks, seed_members, args.max_steps)
        assert all(x >= y for x, y in zip(trail, trail[1:])), f"count rose: {trail}"
        results.append((seed_members, members, trail, converged))
        if (run + 1) % 25 == 0:
            elapsed = time.monotonic() - started
            print(f"  {run + 1:>4}/{args.seeds} climbs ({elapsed:.0f}s elapsed)",
                  flush=True)

    finals = np.array([t[-1] for _, _, t, _ in results])
    order = np.lexsort((np.arange(len(results)), finals))
    best_greedy = results[order[0]][1]
    independent = totalizers_by_engine(*[codon_text(m) for m in best_greedy])
    assert independent == finals[order[0]], (
        f"matrix and engine disagree: {finals[order[0]]} vs {independent}"
    )

    print(f"\n{len(results)} climbs, {sum(1 for _, _, _, c in results if c)} converged "
          f"in {time.monotonic() - started:.0f}s")
    print(f"initial: min {min(t[0] for _, _, t, _ in results)} "
          f"median {int(np.median([t[0] for _, _, t, _ in results]))}")
    print(f"final:   min {finals.min()} median {int(np.median(finals))} "
          f"max {finals.max()}")
    show("Strongest pairs found by hill-climbing:",
         [(int(finals[i]), *results[i][1]) for i in order[: args.top]])

    with open(args.out, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["seed_index", "initial_a", "initial_b", "initial_totalizers",
                         "final_a", "final_b", "final_totalizers", "steps", "converged"])
        for i, (seed_members, members, trail, converged) in enumerate(results):
            writer.writerow([i]
                            + [codon_text(m) for m in seed_members]
                            + [trail[0]]
                            + [codon_text(m) for m in members]
                            + [trail[-1], len(trail) - 1, int(converged)])
    print(f"\npairs -> {args.out}")

    # --- exhaustive sweep -------------------------------------------------
    if args.exhaustive:
        print(f"\nsweeping {len(sorted_codons())} first-members "
              f"(every pair, up to simultaneous permutation)")
        legal, unrestricted = exhaustive(words, masks, args.top)

        count, a, b = legal[0]
        independent = totalizers_by_engine(codon_text(a), codon_text(b))
        assert independent == count, (
            f"best pair: matrix says {count}, engine says {independent}"
        )
        assert not (masks[a] & masks[b]), "best legal pair is not disjoint"
        assert count <= KNOWN_LEGAL_BAR, (
            f"optimum {count} worse than the known legal bar {KNOWN_LEGAL_BAR}"
        )
        assert int(finals.min()) >= count, (
            f"greedy found {finals.min()}, better than the claimed optimum {count}"
        )
        print(f"\nverified: optimum re-counted by replaying matchups ({count}); "
              f"members disjoint; at least as good as the known bar "
              f"{KNOWN_LEGAL_BAR}; no climb beat it")

        show("Best character-disjoint pairs (provable optimum):", legal)
        show("Best pairs with the disjointness rule dropped:", unrestricted)
        gap = int(finals.min()) - count
        print(f"\ngreedy best {int(finals.min())} vs optimum {count}: "
              f"gap {gap} ({gap / max(count, 1):+.1%})")
        print(f"disjointness costs {count - unrestricted[0][0]} totalizers "
              f"({unrestricted[0][0]} unrestricted vs {count} legal)")


if __name__ == "__main__":
    main()
