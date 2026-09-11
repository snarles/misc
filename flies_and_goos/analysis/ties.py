"""Task 1: do non-identical codons tie on all 10 contests?

Prints the counts and a set of verified witnesses. The search itself lives in
``flies_and_goos.ties``; the reduction it uses is documented there.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/ties.py
"""

from flies_and_goos.engine import N_CODONS
from flies_and_goos.game import face_off
from flies_and_goos.tables import ALPHABET, NUM_CONTESTS
from flies_and_goos.ties import (
    ZERO,
    count_tying_pairs,
    find_witnesses,
    pairs_by_signature,
)


def verify(witnesses) -> None:
    """Confirm each witness really draws, via the reference implementation."""
    for a, b in witnesses:
        assert a != b, f"{a} vs {b} is not a non-identical pair"
        result = face_off(a, b)
        assert result.winner is None, f"{a} vs {b} was not a draw"
        assert len(result.rounds) == NUM_CONTESTS, f"{a} vs {b} ended early"


def main() -> None:
    groups = pairs_by_signature()
    total = count_tying_pairs(groups)
    uncontested = len(groups[ZERO]) ** 3

    interchangeable = [p for p in groups[ZERO] if p[0] != p[1]]
    print(f"ordered character pairs:          {len(ALPHABET) ** 2}")
    print(f"distinct signature vectors:       {len(groups)}")
    print(
        f"score-identical pairs:            {len(groups[ZERO])}"
        f"  (interchangeable: {', '.join(interchangeable)})"
    )
    print()
    print(f"ordered codon pairs tying all {NUM_CONTESTS}:  {total}")
    print(f"  identical (A == B):             {N_CODONS}")
    print(f"  every position uncontested:     {uncontested}")
    print(f"  NON-IDENTICAL ties:             {total - N_CODONS}")
    print(f"  ties that trade positions:      {total - uncontested}")
    print()

    witnesses = find_witnesses(groups)
    verify(witnesses)
    print(f"{len(witnesses)} verified witnesses, every position contested:")
    for a, b in witnesses:
        print(f"  {a} vs {b}")


if __name__ == "__main__":
    main()
