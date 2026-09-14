"""Task 4: 3-fortresses - triples of codons that are hard to sweep.

A 3-fortress is three codons no two of which share a character (repeats inside
one codon are fine). A "totalizer" is a codon that beats all three. Strong
fortresses have few totalizers. Nobody knows yet what "few" means, so this
samples random valid triples and hill-climbs them by single-codon replacement
to find out what range is reachable.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/fortress.py [--seeds 100]
"""

import argparse
import csv
import sys
import time
from itertools import permutations

import numpy as np

from flies_and_goos.duo import BeatMatrixMissing, load_beat_matrix
from flies_and_goos.engine import (
    N_CHARS,
    N_CODONS,
    codon_text,
    outcomes_against_all,
)

#: popcount of every byte value, for summing packed rows without unpacking.
#: uint8 keeps the lookup's temporary the same size as the data it indexes.
POPCNT = np.unpackbits(np.arange(256, dtype=np.uint8)[:, None], axis=1).sum(
    axis=1
).astype(np.uint8)

FORTRESS_SIZE = 3

#: Rough cap on the bytes a single candidate-sweep chunk may materialise.
CHUNK_BYTES = 64_000_000

#: Below this fill fraction, gathering only the live byte columns beats
#: scanning whole rows.
GATHER_FRACTION = 0.5

#: Random candidates whose swept count is re-derived directly, as a guard on
#: the sweep that drives every swap.
VERIFY_PROBES = 20


def char_masks() -> np.ndarray:
    """36-bit mask of the distinct characters each codon uses, by codon index.

    Two codons may share a fortress iff their masks are disjoint.
    """
    masks = np.zeros(N_CODONS, dtype=np.int64)
    positions = np.arange(N_CODONS)
    for divisor in (N_CHARS**2, N_CHARS, 1):
        chars = (positions // divisor) % N_CHARS
        masks |= np.int64(1) << chars.astype(np.int64)
    return masks


def totalizer_mask(matrix: np.ndarray, members) -> np.ndarray:
    """Packed bitmask of the codons defeating every member of ``members``.

    Row ``j`` of the matrix lists who beats ``j``, so ANDing the members' rows
    leaves exactly the codons that beat all of them.
    """
    packed = np.asarray(matrix[members[0]]).copy()
    for member in members[1:]:
        packed &= np.asarray(matrix[member])
    return packed


def count_bits(packed: np.ndarray) -> int:
    return int(POPCNT[packed].sum())


def totalizers(matrix: np.ndarray, members) -> int:
    return count_bits(totalizer_mask(matrix, members))


def candidate_counts(matrix: np.ndarray, keep: np.ndarray) -> np.ndarray:
    """For every codon r, how many of ``keep``'s codons also defeat r?

    ``keep`` is the packed totalizer mask of the two members being held fixed,
    so this is the totalizer count of the fortress that would result from
    putting r in the free slot. Only the byte columns where ``keep`` is nonzero
    can contribute, and that set shrinks fast as a fortress improves, so
    restricting to them is what makes the sweep cheap.
    """
    columns = np.flatnonzero(keep)
    counts = np.zeros(N_CODONS, dtype=np.int32)
    if columns.size == 0:
        return counts

    # Gathering columns costs more per byte than reading rows whole, so it only
    # pays once ``keep`` is sparse - which is most of the climb, but not the
    # early steps where nearly every byte holds a set bit.
    gather = columns.size < GATHER_FRACTION * keep.size
    width = columns.size if gather else keep.size
    wanted = keep[columns] if gather else keep

    chunk = max(1, CHUNK_BYTES // width)
    for start in range(0, N_CODONS, chunk):
        rows = matrix[start : start + chunk]
        block = (rows[:, columns] if gather else rows) & wanted
        counts[start : start + chunk] = POPCNT[block].sum(axis=1, dtype=np.int32)
    return counts


def sample_fortress(rng: np.random.Generator, masks: np.ndarray) -> list[int]:
    """A uniformly-sampled member, then uniform among what stays disjoint.

    Not uniform over triples - later slots draw from a smaller pool - but this
    only has to seed the climb.
    """
    members: list[int] = []
    used = np.int64(0)
    for _ in range(FORTRESS_SIZE):
        pool = np.flatnonzero((masks & used) == 0)
        pick = int(rng.choice(pool))
        members.append(pick)
        used |= masks[pick]
    return members


def is_disjoint(members, masks: np.ndarray) -> bool:
    used = np.int64(0)
    for member in members:
        if masks[member] & used:
            return False
        used |= masks[member]
    return True


def best_swap(matrix, masks, members, current):
    """The single replacement that most reduces the totalizer count.

    Returns ``(slot, codon, count)`` or ``None`` when nothing improves.
    """
    best = None
    for slot in range(FORTRESS_SIZE):
        others = [m for i, m in enumerate(members) if i != slot]
        keep = totalizer_mask(matrix, others)
        counts = candidate_counts(matrix, keep)

        blocked = masks[others[0]] | masks[others[1]]
        counts[(masks & blocked) != 0] = np.iinfo(np.int32).max

        # Lowest count, ties broken by codon index, matching the house
        # lexsort idiom in winrates.py and partner.py.
        pick = int(np.lexsort((np.arange(N_CODONS), counts))[0])
        score = int(counts[pick])
        if score < current and (best is None or score < best[2]):
            best = (slot, pick, score)
    return best


def climb(matrix, masks, members, max_steps):
    """Apply the best improving swap until none exists. Returns the trajectory."""
    members = list(members)
    trail = [totalizers(matrix, members)]
    for _ in range(max_steps):
        move = best_swap(matrix, masks, members, trail[-1])
        if move is None:
            return members, trail, True
        slot, codon, score = move
        members[slot] = codon
        assert is_disjoint(members, masks), f"swap broke disjointness: {members}"
        assert score < trail[-1], f"accepted a non-improving swap: {score}"
        trail.append(score)
    return members, trail, False


def totalizers_by_engine(members) -> int:
    """Recount totalizers by replaying matchups, independent of the matrix."""
    beaten = outcomes_against_all(codon_text(members[0])) < 0
    for member in members[1:]:
        beaten &= outcomes_against_all(codon_text(member)) < 0
    return int(np.count_nonzero(beaten))


def canonical(members) -> tuple[str, ...]:
    """Orbit representative under simultaneous position permutation.

    Permuting all three members the same way leaves the totalizer count
    unchanged, so distinct local optima must be counted up to this symmetry.
    """
    texts = [codon_text(m) for m in members]
    orbit = [
        tuple(sorted("".join(t[i] for i in order) for t in texts))
        for order in permutations(range(FORTRESS_SIZE))
    ]
    return min(orbit)


def show(title, rows):
    print(f"\n{title}")
    print(f"  {'fortress':20} {'total':>6} {'from':>6} {'steps':>6}")
    for texts, final, initial, steps in rows:
        print(f"  {' '.join(texts):20} {final:>6} {initial:>6} {steps:>6}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seeds", type=int, default=100)
    parser.add_argument("--rng-seed", type=int, default=20260914)
    parser.add_argument("--max-steps", type=int, default=50)
    parser.add_argument("--out", default="fortresses.csv")
    parser.add_argument("--top", type=int, default=15)
    args = parser.parse_args()

    try:
        # np.array, not asarray: load_beat_matrix memory-maps, and the climb
        # re-scans the whole thing thousands of times.
        matrix = np.array(load_beat_matrix())
    except BeatMatrixMissing as error:
        sys.exit(str(error))

    print(f"beat matrix in memory ({matrix.nbytes / 1e6:.0f} MB)")
    masks = char_masks()
    rng = np.random.default_rng(args.rng_seed)

    results = []
    started = time.monotonic()
    for run in range(args.seeds):
        seed_members = sample_fortress(rng, masks)
        assert is_disjoint(seed_members, masks), "sampler broke disjointness"
        members, trail, converged = climb(matrix, masks, seed_members, args.max_steps)
        assert all(a >= b for a, b in zip(trail, trail[1:])), (
            f"totalizer count rose along the climb: {trail}"
        )
        results.append((seed_members, members, trail, converged))

        if (run + 1) % 10 == 0:
            elapsed = time.monotonic() - started
            rate = (run + 1) / elapsed
            print(
                f"  {run + 1:>4}/{args.seeds} seeds "
                f"({elapsed:.0f}s elapsed, {(args.seeds - run - 1) / rate:.0f}s left)",
                flush=True,
            )

    finals = np.array([trail[-1] for _, _, trail, _ in results])
    initials = np.array([trail[0] for _, _, trail, _ in results])
    order = np.lexsort((np.arange(len(results)), finals))

    # --- verification -----------------------------------------------------
    best_members = results[order[0]][1]
    independent = totalizers_by_engine(best_members)
    assert independent == finals[order[0]], (
        f"matrix and engine disagree: {finals[order[0]]} vs {independent}"
    )
    for members in (results[order[0]][1], results[order[-1]][1]):
        triple = totalizers(matrix, members)
        for pair in ((0, 1), (0, 2), (1, 2)):
            duo = totalizers(matrix, [members[i] for i in pair])
            assert triple <= duo, f"triple {triple} exceeds its pair {duo}"
        beats_all = np.unpackbits(totalizer_mask(matrix, members))[:N_CODONS]
        for member in members:
            assert not beats_all[member], "a member totalizes its own fortress"
    assert all(is_disjoint(m, masks) for _, m, _, _ in results)

    # The swept counts drive every swap, so a wrong sweep would steer the climb
    # while leaving the reported totals self-consistent. Check the sweep itself
    # against a direct per-triple count.
    others = best_members[1:]
    swept = candidate_counts(matrix, totalizer_mask(matrix, others))
    probes = rng.choice(
        np.flatnonzero((masks & (masks[others[0]] | masks[others[1]])) == 0),
        size=VERIFY_PROBES,
        replace=False,
    )
    for probe in probes:
        direct = totalizers(matrix, [int(probe)] + others)
        assert swept[probe] == direct, (
            f"sweep disagrees for {codon_text(int(probe))}: "
            f"{swept[probe]} vs {direct}"
        )

    print(
        f"\nverified: best fortress re-counted by replaying matchups "
        f"({independent} totalizers); candidate sweep matches direct counts on "
        f"{VERIFY_PROBES} probes; every triple is at most as beatable as its "
        f"pairs; no member totalizes its own fortress; all "
        f"{len(results)} triples character-disjoint"
    )

    n_converged = sum(1 for _, _, _, c in results if c)
    print(
        f"\n{len(results)} seeds, {n_converged} converged to a local optimum "
        f"in {time.monotonic() - started:.0f}s"
    )
    print(f"initial totalizers: min {initials.min()} median "
          f"{int(np.median(initials))} max {initials.max()}")
    print(f"final totalizers:   min {finals.min()} median "
          f"{int(np.median(finals))} max {finals.max()}")

    orbits = {canonical(members) for _, members, _, _ in results}
    print(f"distinct local optima up to simultaneous permutation: {len(orbits)}")

    rows = [
        (
            [codon_text(m) for m in results[i][1]],
            int(finals[i]),
            int(initials[i]),
            len(results[i][2]) - 1,
        )
        for i in order[: args.top]
    ]
    show(f"Strongest fortresses found (of {len(results)} climbs):", rows)

    with open(args.out, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(
            [
                "seed_index", "initial_a", "initial_b", "initial_c",
                "initial_totalizers", "final_a", "final_b", "final_c",
                "final_totalizers", "steps", "converged",
            ]
        )
        for i, (seed_members, members, trail, converged) in enumerate(results):
            writer.writerow(
                [i]
                + [codon_text(m) for m in seed_members]
                + [trail[0]]
                + [codon_text(m) for m in members]
                + [trail[-1], len(trail) - 1, int(converged)]
            )
    print(f"\nfortresses -> {args.out}")


if __name__ == "__main__":
    main()
