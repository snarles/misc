"""Guards on the beat-matrix orientation, which a transposed read would break
silently rather than loudly.
"""

import os

import numpy as np
import pytest

from flies_and_goos.duo import (
    MATRIX_PATH,
    coverage_by_sweep,
    coverage_counts,
    defeated_by,
    defeats,
    load_beat_matrix,
)
from flies_and_goos.engine import N_CODONS, codon_index, codon_text, outcomes_against_all

needs_matrix = pytest.mark.skipif(
    not os.path.exists(MATRIX_PATH),
    reason=f"{MATRIX_PATH} not built; run analysis/winrates.py",
)


def test_defeated_by_and_defeats_are_disjoint():
    beaten_by = set(defeated_by("ENP").tolist())
    beats = set(defeats("ENP").tolist())
    assert not beaten_by & beats
    assert codon_index("ENP") not in beaten_by | beats  # draws with itself


def test_defeats_agrees_with_outcomes():
    outcomes = outcomes_against_all("R2D")
    assert np.array_equal(defeats("R2D"), np.flatnonzero(outcomes > 0))
    assert np.array_equal(defeated_by("R2D"), np.flatnonzero(outcomes < 0))


@needs_matrix
def test_matrix_row_lists_who_beats_that_codon():
    matrix = load_beat_matrix()
    for text in ("ENP", "PGE", "R2D"):
        row = np.unpackbits(np.asarray(matrix[codon_index(text)]))[:N_CODONS]
        assert np.array_equal(np.flatnonzero(row), defeated_by(text))


@needs_matrix
def test_coverage_matches_independent_sweep():
    matrix = load_beat_matrix()
    targets = defeated_by("ENP")
    coverage = coverage_counts(targets, matrix)

    order = np.lexsort((np.arange(N_CODONS), -coverage))
    sample = [codon_text(i) for i in list(order[:5]) + list(order[-3:])]
    independent = coverage_by_sweep(targets, sample)
    assert np.array_equal(
        independent, coverage[[codon_index(t) for t in sample]]
    )


@needs_matrix
def test_codon_covers_none_of_its_own_conquerors():
    matrix = load_beat_matrix()
    for text in ("ENP", "R2D"):
        coverage = coverage_counts(defeated_by(text), matrix)
        assert coverage[codon_index(text)] == 0


@needs_matrix
def test_coverage_and_shared_losses_account_for_every_target():
    """coverage + beaten-by-both + drawn-within == |targets|."""
    matrix = load_beat_matrix()
    targets = defeated_by("ENP")
    coverage = coverage_counts(targets, matrix)

    for text in ("EGP", "071", "ZZZ"):
        index = codon_index(text)
        outcomes = outcomes_against_all(text)[targets]
        assert int(coverage[index]) == int(np.count_nonzero(outcomes > 0))
        assert (
            int(np.count_nonzero(outcomes > 0))
            + int(np.count_nonzero(outcomes < 0))
            + int(np.count_nonzero(outcomes == 0))
            == len(targets)
        )


@needs_matrix
def test_best_partner_for_enp_is_egp():
    """Reported in report.md; EGP also minimises the codons beating both."""
    matrix = load_beat_matrix()
    coverage = coverage_counts(defeated_by("ENP"), matrix)
    assert codon_text(int(coverage.argmax())) == "EGP"
    assert int(coverage.max()) == 15281


@needs_matrix
def test_partner_coverage_is_not_permutation_invariant():
    """Unlike P(win), which report.md shows is exactly permutation-invariant."""
    matrix = load_beat_matrix()
    coverage = coverage_counts(defeated_by("ENP"), matrix)
    scores = {
        variant: int(coverage[codon_index(variant)])
        for variant in ("EGP", "EPG", "GEP", "GPE", "PEG", "PGE")
    }
    assert len(set(scores.values())) > 1, scores
