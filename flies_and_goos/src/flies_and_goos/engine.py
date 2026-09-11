"""Vectorized face-off engine: one codon against the whole codon space at once.

``game.py`` stays the readable reference implementation; this module is the fast
path used by the analysis scripts, and ``tests/test_engine.py`` checks the two
agree. Codons are addressed by index, ``i1 * 36**2 + i2 * 36 + i3`` over
``tables.ALPHABET``, which is plain ``itertools.product`` order.
"""

import numpy as np

from .game import CODON_LENGTH, Codon
from .tables import ALPHABET, CONTESTS, NUM_CONTESTS, STRAIGHT

N_CHARS = len(ALPHABET)
N_CODONS = N_CHARS**CODON_LENGTH

CHAR_INDEX = {ch: i for i, ch in enumerate(ALPHABET)}

#: (10, 36) int8 - every contest's score for every character.
SCORES = np.array(
    [[table[ch] for ch in ALPHABET] for _, table in CONTESTS], dtype=np.int8
)

IS_STRAIGHT = np.array([ch in STRAIGHT for ch in ALPHABET])

_index = np.arange(N_CODONS)
_POSITIONS = (
    _index // N_CHARS**2,
    (_index // N_CHARS) % N_CHARS,
    _index % N_CHARS,
)

#: Stability of every codon, in index order.
STABILITY_ALL = sum(SCORES[0][p].astype(np.int16) for p in _POSITIONS)

#: A codon is a Fly when straight characters outnumber curved ones, i.e. when
#: at least 2 of its 3 characters are straight.
IS_FLY_ALL = sum(IS_STRAIGHT[p].astype(np.int8) for p in _POSITIONS) >= 2

del _index


def codon_index(codon: Codon | str) -> int:
    """Index of a codon in the engine's ordering."""
    text = codon.text if isinstance(codon, Codon) else Codon(codon).text
    result = 0
    for ch in text:
        result = result * N_CHARS + CHAR_INDEX[ch]
    return result


def codon_text(index: int) -> str:
    """Inverse of :func:`codon_index`."""
    return "".join(
        ALPHABET[(index // N_CHARS**power) % N_CHARS]
        for power in reversed(range(CODON_LENGTH))
    )


def all_codon_texts() -> list[str]:
    return [codon_text(i) for i in range(N_CODONS)]


def outcomes_against_all(codon: Codon | str) -> np.ndarray:
    """Play ``codon`` against every codon, returning an int8 array of length
    ``N_CODONS``: ``+1`` where ``codon`` wins, ``-1`` where the opponent wins,
    ``0`` where all 10 contests tie.
    """
    text = codon.text if isinstance(codon, Codon) else Codon(codon).text
    chars = [CHAR_INDEX[ch] for ch in text]

    # Per contest, the outcome against every opponent in one broadcast: the
    # per-position sign vectors depend only on the opponent's character in that
    # position, so summing them across the three axes covers all 46656 at once.
    contest_outcomes = np.empty((NUM_CONTESTS, N_CODONS), dtype=np.int8)
    for contest in range(NUM_CONTESTS):
        scores = SCORES[contest]
        signs = [np.sign(scores[ch] - scores) for ch in chars]
        total = (
            signs[0][:, None, None]
            + signs[1][None, :, None]
            + signs[2][None, None, :]
        )
        contest_outcomes[contest] = np.sign(total).reshape(-1)

    # Friend (a Fly against a Goo) inverts every contest: lower score wins.
    is_fly = sum(IS_STRAIGHT[ch] for ch in chars) >= 2
    friendly = IS_FLY_ALL != is_fly
    signed = np.where(friendly, -contest_outcomes, contest_outcomes)

    # Start at the units digit of the combined stability, then step forward
    # through the contests, locking in the first one that is not a tie.
    stability = int(SCORES[0][chars].sum())
    start = (stability + STABILITY_ALL) % NUM_CONTESTS
    columns = np.arange(N_CODONS)
    result = np.zeros(N_CODONS, dtype=np.int8)
    for step in range(NUM_CONTESTS):
        candidate = signed[(start + step) % NUM_CONTESTS, columns]
        np.copyto(result, candidate, where=result == 0)
    return result
