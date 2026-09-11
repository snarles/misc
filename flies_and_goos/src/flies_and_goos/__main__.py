"""Print a round-by-round face-off: ``python -m flies_and_goos R2D C3P``."""

import argparse

from .game import Codon, FaceOff, face_off


def format_faceoff(result: FaceOff) -> str:
    lines = []
    for codon in (result.a, result.b):
        lines.append(f"{codon}  {codon.type}, stability {codon.stability}")
    lines.append(f"Relationship: {result.relationship} "
                 f"({'lower' if result.friendly else 'higher'} is better)")
    lines.append("")

    for round_ in result.rounds:
        lines.append(f"Contest #{round_.contest}: {round_.name}")
        for codon, scores in (
            (result.a, round_.a_scores),
            (result.b, round_.b_scores),
        ):
            spread = "  ".join(
                f"{ch}={score}" for ch, score in zip(codon.text, scores)
            )
            lines.append(f"  {codon}  {spread}")
        lines.append(
            f"  positions: {result.a} {round_.a_positions}"
            f" - {round_.b_positions} {result.b}"
            + ("  (tie, on to the tiebreaker)" if round_.winner is None else "")
        )
        lines.append("")

    if result.winner is None:
        lines.append("All 10 contests tied: no winner.")
    else:
        lines.append(f"Winner: {result.winner}")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> None:
    parser = argparse.ArgumentParser(prog="flies_and_goos", description=__doc__)
    parser.add_argument("codon_a", type=Codon)
    parser.add_argument("codon_b", type=Codon)
    args = parser.parse_args(argv)
    print(format_faceoff(face_off(args.codon_a, args.codon_b)))


if __name__ == "__main__":
    main()
