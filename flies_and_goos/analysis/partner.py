"""Find the best partner for a codon: the codon defeating the most of its
conquerors.

ENP is the strongest codon against a uniform opponent, but 17,287 codons still
beat it. This asks which single codon cleans up the most of that set.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py [--codon ENP]
"""

import argparse
import sys
from itertools import permutations

import numpy as np

from flies_and_goos.duo import (
    BeatMatrixMissing,
    coverage_by_sweep,
    coverage_counts,
    defeated_by,
    load_beat_matrix,
)
from flies_and_goos.engine import N_CODONS, codon_index, codon_text

VERIFY_TOP = 10


def report_rows(indexes, coverage, targets_count, pwin, matrix, targets):
    """coverage, team losses and P(win) for each candidate."""
    rows = []
    for index in indexes:
        text = codon_text(index)
        # Codons beating both = targets that also defeat this candidate.
        also_lost = int(
            np.count_nonzero(
                np.unpackbits(np.asarray(matrix[index]))[:N_CODONS][targets]
            )
        )
        rows.append(
            (
                text,
                int(coverage[index]),
                int(coverage[index]) / targets_count,
                also_lost,
                pwin.get(text, float("nan")),
            )
        )
    return rows


def show(title, rows):
    print(f"\n{title}")
    print(f"  {'codon':6} {'covers':>7} {'frac':>7} {'beat both':>10} {'P(win)':>7}")
    for text, covers, frac, both, p in rows:
        print(f"  {text:6} {covers:>7} {frac:>7.4f} {both:>10} {p:>7.4f}")


def load_pwin(path="winrates.tsv"):
    try:
        with open(path) as handle:
            next(handle)
            return {
                line.split("\t")[0]: float(line.split("\t")[6])
                for line in handle
            }
    except OSError:
        return {}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--codon", default="ENP")
    parser.add_argument("--top", type=int, default=15)
    args = parser.parse_args()

    try:
        matrix = load_beat_matrix()
    except BeatMatrixMissing as error:
        sys.exit(str(error))

    hero = args.codon.upper()
    targets = defeated_by(hero)
    n_targets = len(targets)
    print(f"{hero} is defeated by {n_targets} codons ({n_targets / N_CODONS:.4f})")

    coverage = coverage_counts(targets, matrix)

    # --- verification -----------------------------------------------------
    order = np.lexsort((np.arange(N_CODONS), -coverage))
    top = order[:VERIFY_TOP]
    independent = coverage_by_sweep(targets, [codon_text(i) for i in top])
    assert np.array_equal(independent, coverage[top]), (
        f"matrix and engine disagree: {coverage[top]} vs {independent}"
    )
    assert coverage[codon_index(hero)] == 0, (
        f"{hero} should defeat none of its own conquerors"
    )
    print(
        f"verified: top {VERIFY_TOP} re-derived by replaying matchups; "
        f"coverage[{hero}] == 0; mean coverage {coverage.mean():.0f} "
        f"(~half of {n_targets} expected)"
    )

    pwin = load_pwin()
    show(
        f"Best partners for {hero} (defeat the most of its {n_targets} conquerors):",
        report_rows(order[: args.top], coverage, n_targets, pwin, matrix, targets),
    )
    show(
        f"Worst partners for {hero}:",
        report_rows(order[-5:], coverage, n_targets, pwin, matrix, targets),
    )

    # --- permutation sensitivity -----------------------------------------
    best = codon_text(order[0])
    variants = sorted({"".join(p) for p in permutations(best)})
    print(f"\nPermutations of the best partner {best} (P(win) is invariant to"
          f" these; coverage is not):")
    for variant in variants:
        index = codon_index(variant)
        print(f"  {variant}  covers {int(coverage[index]):>6}"
              f"  ({int(coverage[index]) / n_targets:.4f})")

    hero_perms = sorted({"".join(p) for p in permutations(hero)})
    spread = [int(coverage[codon_index(v)]) for v in variants]
    print(
        f"\ncoverage spread across those permutations: "
        f"{min(spread)}..{max(spread)} "
        f"({'NOT permutation-invariant' if min(spread) != max(spread) else 'invariant'})"
    )
    print(f"(for reference, {hero}'s own permutations are {', '.join(hero_perms)})")


if __name__ == "__main__":
    main()
