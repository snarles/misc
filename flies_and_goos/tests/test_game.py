import pytest

from flies_and_goos import tables
from flies_and_goos.game import (
    FOE,
    FRIEND,
    FLY,
    GOO,
    Codon,
    face_off,
    opening_contest,
)

# The 10 supplied test cases: (codon a, codon b, winner).
CASES = [
    ("R2D", "C3P", "R2D"),
    ("H2O", "CO2", "H2O"),
    ("B2B", "P2P", "B2B"),
    ("2GO", "G2G", "G2G"),
    ("4GT", "4GV", "4GT"),
    ("B12", "A1C", "A1C"),
    ("F16", "F35", "F35"),
    ("GR8", "3PO", "GR8"),
    ("B4U", "N95", "N95"),
    ("1UP", "Y2K", "1UP"),
]


@pytest.mark.parametrize("a,b,expected", CASES)
def test_supplied_cases(a, b, expected):
    assert str(face_off(a, b).winner) == expected


@pytest.mark.parametrize("a,b,expected", CASES)
def test_supplied_cases_are_symmetric(a, b, expected):
    """Swapping the players must not change who wins."""
    assert str(face_off(b, a).winner) == expected


@pytest.mark.parametrize("score_table", [t for _, t in tables.CONTESTS])
def test_every_contest_scores_every_character(score_table):
    assert score_table.keys() == set(tables.ALPHABET)


def test_curved_and_straight_partition_the_alphabet():
    assert tables.CURVED | tables.STRAIGHT == set(tables.ALPHABET)
    assert not tables.CURVED & tables.STRAIGHT


def test_stability_table_matches_contest_zero():
    assert tables.CONTESTS[0][1] is tables.STABILITY


@pytest.mark.parametrize(
    "text,expected_type,expected_stability",
    [
        ("IGW", FLY, 7),
        ("U2B", GOO, 7),
        ("A1S", FLY, 6),
        ("98F", GOO, 2),
    ],
)
def test_worked_examples_type_and_stability(
    text, expected_type, expected_stability
):
    codon = Codon(text)
    assert codon.type == expected_type
    assert codon.stability == expected_stability


def test_intro_example_needs_a_tiebreaker():
    """IGW vs U2B: ties on contest #4, then Phil's U2B takes contest #5."""
    result = face_off("IGW", "U2B")
    assert result.relationship == FRIEND
    assert opening_contest(result.a, result.b) == 4

    first, second = result.rounds
    assert (first.contest, first.a_positions, first.b_positions) == (4, 1, 1)
    assert first.winner is None
    assert (second.contest, second.a_positions, second.b_positions) == (5, 0, 1)
    assert str(result.winner) == "U2B"


def test_rules_example_decided_on_first_contest():
    """A1S vs 98F: stability 6 + 2 picks contest #8, won 2-1 on side bumps."""
    result = face_off("A1S", "98F")
    assert result.relationship == FRIEND
    assert len(result.rounds) == 1

    round_ = result.rounds[0]
    assert round_.contest == 8
    assert round_.a_scores == (0, 0, 2)
    assert round_.b_scores == (2, 4, 0)
    assert (round_.a_positions, round_.b_positions) == (2, 1)
    assert str(result.winner) == "A1S"


def test_same_type_codons_are_foes_and_higher_wins():
    result = face_off("AAA", "AAN")
    assert result.relationship == FOE
    # Both Flies, stability 6 and 6 -> contest #2 (V-junctions): A=1, N=2.
    assert result.rounds[0].contest == 2
    assert str(result.winner) == "AAN"


def test_identical_codons_tie_every_contest():
    result = face_off("ABC", "ABC")
    assert result.winner is None
    assert result.winner_index is None
    assert len(result.rounds) == tables.NUM_CONTESTS
    assert {r.contest for r in result.rounds} == set(range(tables.NUM_CONTESTS))


def test_codon_is_normalized():
    assert Codon(" r2d ").text == "R2D"


@pytest.mark.parametrize("bad", ["AB", "ABCD", "A B", "A-B", ""])
def test_invalid_codons_are_rejected(bad):
    with pytest.raises(ValueError):
        Codon(bad)
