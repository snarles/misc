"""The Flies and Goos game rules.

A codon is 3 characters drawn from A-Z and 0-9. Two codons face off: their
types decide whether the contest is friendly (lower score takes a position) or
unfriendly (higher takes it), their combined stability picks the opening
contest, and ties push the contest number forward by one until someone wins or
all 10 contests have been used.
"""

from dataclasses import dataclass

from .tables import ALPHABET, CONTESTS, NUM_CONTESTS, STABILITY, STRAIGHT

CODON_LENGTH = 3

FLY = "Fly"
GOO = "Goo"
FRIEND = "Friend"
FOE = "Foe"


@dataclass(frozen=True)
class Codon:
    """A 3-character codon. Construction validates and upper-cases the text."""

    text: str

    def __post_init__(self):
        cleaned = self.text.strip().upper()
        if len(cleaned) != CODON_LENGTH:
            raise ValueError(
                f"a codon is {CODON_LENGTH} characters, got {self.text!r}"
            )
        bad = [ch for ch in cleaned if ch not in ALPHABET]
        if bad:
            raise ValueError(
                f"{self.text!r} uses characters outside A-Z and 0-9: "
                f"{''.join(bad)}"
            )
        object.__setattr__(self, "text", cleaned)

    @property
    def type(self) -> str:
        """``FLY`` if straight characters outnumber curved ones, else ``GOO``."""
        straight = sum(ch in STRAIGHT for ch in self.text)
        return FLY if straight > CODON_LENGTH - straight else GOO

    @property
    def stability(self) -> int:
        """Sum of the per-character stability indices (contest #0 scores)."""
        return sum(STABILITY[ch] for ch in self.text)

    def scores(self, contest: int) -> tuple[int, ...]:
        """This codon's per-character scores in the given contest."""
        table = CONTESTS[contest][1]
        return tuple(table[ch] for ch in self.text)

    def __str__(self) -> str:
        return self.text


@dataclass(frozen=True)
class Round:
    """One contest between two codons."""

    contest: int
    name: str
    a_scores: tuple[int, ...]
    b_scores: tuple[int, ...]
    a_positions: int
    b_positions: int
    winner: int | None
    """0 if the first codon took more positions, 1 if the second, else None."""


@dataclass(frozen=True)
class FaceOff:
    """The full result of pitting two codons against each other."""

    a: Codon
    b: Codon
    relationship: str
    rounds: tuple[Round, ...]
    winner: Codon | None
    winner_index: int | None
    """0 if ``a`` won, 1 if ``b`` won, None if all 10 contests tied."""

    @property
    def friendly(self) -> bool:
        return self.relationship == FRIEND


def relationship(a: Codon, b: Codon) -> str:
    """``FRIEND`` for a Fly against a Goo, ``FOE`` for two of a kind."""
    return FOE if a.type == b.type else FRIEND


def opening_contest(a: Codon, b: Codon) -> int:
    """The units digit of the two codons' combined stability."""
    return (a.stability + b.stability) % NUM_CONTESTS


def play_round(a: Codon, b: Codon, contest: int, friendly: bool) -> Round:
    """Run a single contest. A tied position is taken by neither player."""
    name, _ = CONTESTS[contest]
    a_scores = a.scores(contest)
    b_scores = b.scores(contest)

    a_positions = b_positions = 0
    for x, y in zip(a_scores, b_scores):
        if x == y:
            continue
        if (x < y) if friendly else (x > y):
            a_positions += 1
        else:
            b_positions += 1

    if a_positions > b_positions:
        winner = 0
    elif b_positions > a_positions:
        winner = 1
    else:
        winner = None

    return Round(
        contest=contest,
        name=name,
        a_scores=a_scores,
        b_scores=b_scores,
        a_positions=a_positions,
        b_positions=b_positions,
        winner=winner,
    )


def face_off(a: Codon | str, b: Codon | str) -> FaceOff:
    """Play two codons against each other, tiebreakers included."""
    a = a if isinstance(a, Codon) else Codon(a)
    b = b if isinstance(b, Codon) else Codon(b)

    status = relationship(a, b)
    friendly = status == FRIEND
    start = opening_contest(a, b)

    rounds: list[Round] = []
    winner_index: int | None = None
    for offset in range(NUM_CONTESTS):
        current = play_round(a, b, (start + offset) % NUM_CONTESTS, friendly)
        rounds.append(current)
        if current.winner is not None:
            winner_index = current.winner
            break

    return FaceOff(
        a=a,
        b=b,
        relationship=status,
        rounds=tuple(rounds),
        winner=None if winner_index is None else (a, b)[winner_index],
        winner_index=winner_index,
    )
