"""Exact search for codon pairs that tie all 10 contests.

``rules.docx`` lists it as unknown whether non-identical codons can tie every
contest. One reduction makes it decidable in under a second rather than by
sweeping 46656^2 matchups:

A contest is tied when both players take the same number of positions. Friend
gives a position to the *lower* score and Foe to the *higher*, so either way the
tie condition is ``#{a<b} == #{a>b}`` - relationship status cannot affect
whether a matchup is a draw. Writing ``v(x, y)`` for the 10-vector of
``sign(score_c(x) - score_c(y))``, codons A and B tie every contest iff

    v(a1, b1) + v(a2, b2) + v(a3, b3) = 0   (in Z^10)

So it is enough to enumerate the signature vectors of the 1296 ordered character
pairs and count triples of them summing to zero.
"""

from collections import defaultdict
from itertools import product

from .tables import ALPHABET, CONTESTS, NUM_CONTESTS

ZERO = (0,) * NUM_CONTESTS

Signature = tuple[int, ...]


def signature(x: str, y: str) -> Signature:
    """Per-contest sign of ``x``'s score minus ``y``'s."""
    return tuple(
        (table[x] > table[y]) - (table[x] < table[y]) for _, table in CONTESTS
    )


def negate(sig: Signature) -> Signature:
    return tuple(-value for value in sig)


def pairs_by_signature() -> dict[Signature, list[str]]:
    """Every ordered character pair ``x + y``, grouped by its signature."""
    groups: dict[Signature, list[str]] = defaultdict(list)
    for x, y in product(ALPHABET, repeat=2):
        groups[signature(x, y)].append(x + y)
    return groups


def count_tying_pairs(groups: dict[Signature, list[str]]) -> int:
    """Number of ordered codon pairs (A, B) that tie all 10 contests."""
    counts = {sig: len(members) for sig, members in groups.items()}
    # Fold positions 1 and 2, then match position 3 against the negation.
    folded: dict[Signature, int] = defaultdict(int)
    for sig_a, count_a in counts.items():
        for sig_b, count_b in counts.items():
            key = tuple(a + b for a, b in zip(sig_a, sig_b))
            folded[key] += count_a * count_b
    return sum(
        folded.get(negate(sig), 0) * count for sig, count in counts.items()
    )


def find_witnesses(
    groups: dict[Signature, list[str]],
    limit: int = 12,
    contested_only: bool = True,
) -> list[tuple[str, str]]:
    """Sample tying codon pairs.

    With ``contested_only``, every position must be genuinely contested - no
    position may use a score-identical character pair - so the draw comes from
    positions actually trading places rather than from interchangeable glyphs.
    """
    candidates = [sig for sig in groups if not contested_only or sig != ZERO]

    witnesses = []
    for sig_a, sig_b in product(candidates, repeat=2):
        needed = negate(tuple(a + b for a, b in zip(sig_a, sig_b)))
        if needed not in groups or (contested_only and needed == ZERO):
            continue
        first, second, third = (
            groups[sig_a][0],
            groups[sig_b][0],
            groups[needed][0],
        )
        witnesses.append(
            (first[0] + second[0] + third[0], first[1] + second[1] + third[1])
        )
        if len(witnesses) >= limit:
            break
    return witnesses
