"""The analysis rests on ``engine`` agreeing with the reference ``game`` rules."""

import random

import pytest

from flies_and_goos import engine
from flies_and_goos.game import Codon, face_off

from test_game import CASES


def reference_outcome(a: str, b: str) -> int:
    """+1 if ``a`` wins, -1 if ``b`` wins, 0 for a draw - via ``game.face_off``."""
    result = face_off(a, b)
    return 0 if result.winner_index is None else (1, -1)[result.winner_index]


def test_index_round_trips():
    for index in (0, 1, 35, 36, 1295, engine.N_CODONS - 1):
        assert engine.codon_index(engine.codon_text(index)) == index


def test_index_ordering_is_alphabet_order():
    assert engine.codon_text(0) == "000"
    assert engine.codon_text(engine.N_CODONS - 1) == "ZZZ"
    assert engine.codon_index("00Z") == 35


@pytest.mark.parametrize("a,b,expected", CASES)
def test_engine_matches_supplied_cases(a, b, expected):
    outcome = engine.outcomes_against_all(a)[engine.codon_index(b)]
    winner = a if outcome > 0 else b
    assert outcome != 0
    assert winner == expected


def test_engine_matches_reference_on_random_matchups():
    rng = random.Random(20260911)
    texts = engine.all_codon_texts()
    for _ in range(2000):
        a = rng.choice(texts)
        b = rng.choice(texts)
        got = int(engine.outcomes_against_all(a)[engine.codon_index(b)])
        assert got == reference_outcome(a, b), f"{a} vs {b}"


def test_engine_is_antisymmetric():
    rng = random.Random(11092026)
    texts = engine.all_codon_texts()
    for _ in range(200):
        a, b = rng.choice(texts), rng.choice(texts)
        forward = int(engine.outcomes_against_all(a)[engine.codon_index(b)])
        backward = int(engine.outcomes_against_all(b)[engine.codon_index(a)])
        assert forward == -backward, f"{a} vs {b}"


def test_engine_agrees_on_derived_quantities():
    rng = random.Random(7)
    for _ in range(200):
        index = rng.randrange(engine.N_CODONS)
        codon = Codon(engine.codon_text(index))
        assert int(engine.STABILITY_ALL[index]) == codon.stability
        assert bool(engine.IS_FLY_ALL[index]) == (codon.type == "Fly")


def test_best_codon_loses_to_a_near_worst_codon():
    """Non-transitivity, as reported in report.md: BFN has the highest P(win)
    of any codon (0.6925) yet loses head-to-head to AFF, whose 0.3693 is the
    second-lowest of all 8,436 multisets.
    """
    outcome = engine.outcomes_against_all("BFN")[engine.codon_index("AFF")]
    assert outcome == -1
    assert reference_outcome("BFN", "AFF") == -1


def test_beat_relation_contains_cycles():
    """A strict ranking of codons cannot exist: HB1 > VTS > GKB > HB1."""
    cycle = ("HB1", "VTS", "GKB")
    for attacker, defender in zip(cycle, cycle[1:] + cycle[:1]):
        assert reference_outcome(attacker, defender) == 1, f"{attacker}>{defender}"


def test_codon_draws_against_itself():
    for text in ("R2D", "ABC", "000"):
        assert engine.outcomes_against_all(text)[engine.codon_index(text)] == 0
