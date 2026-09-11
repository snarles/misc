"""Regression guard on the all-contest tie search (task 1)."""

from flies_and_goos.engine import N_CODONS
from flies_and_goos.game import face_off
from flies_and_goos.tables import NUM_CONTESTS
from flies_and_goos.ties import (
    ZERO,
    count_tying_pairs,
    find_witnesses,
    pairs_by_signature,
    signature,
)

#: Ordered codon pairs that tie all 10 contests. Independently confirmed by the
#: full 46656 x 46656 sweep in analysis/winrates.py, which counts the same draws
#: by actually playing every matchup.
TYING_ORDERED_PAIRS = 423580


def test_tie_count_matches_full_sweep():
    assert count_tying_pairs(pairs_by_signature()) == TYING_ORDERED_PAIRS


def test_non_identical_ties_exist():
    """The question rules.docx leaves open."""
    assert TYING_ORDERED_PAIRS > N_CODONS


def test_only_two_pairs_of_characters_are_interchangeable():
    groups = pairs_by_signature()
    swaps = {frozenset(p) for p in groups[ZERO] if p[0] != p[1]}
    assert swaps == {frozenset("69"), frozenset("OS")}


def test_signature_is_antisymmetric():
    assert signature("A", "B") == tuple(-v for v in signature("B", "A"))
    assert signature("A", "A") == ZERO


def test_witnesses_really_draw():
    witnesses = find_witnesses(pairs_by_signature(), limit=25)
    assert len(witnesses) == 25
    for a, b in witnesses:
        assert a != b
        result = face_off(a, b)
        assert result.winner is None, f"{a} vs {b}"
        assert len(result.rounds) == NUM_CONTESTS


def test_contested_witnesses_share_no_interchangeable_positions():
    """A contested witness must not lean on 6/9 or O/S substitution."""
    for a, b in find_witnesses(pairs_by_signature(), limit=25):
        assert all(signature(x, y) != ZERO for x, y in zip(a, b))
