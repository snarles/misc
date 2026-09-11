"""Task 2: every codon's record against a uniformly random opponent.

Plays all 46656 codons against all 46656 codons using the vectorized engine
(one codon against the whole space per call), and writes ``winrates.tsv``.

Also saves the bit-packed "who beats whom" matrix to ``beat_matrix.npy``: row
``i`` has bit ``j`` set when codon ``j`` defeats codon ``i``. That is the input
a two-codon defensive-duo search would need, so the sweep does not have to be
repeated later.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/winrates.py
"""

import argparse
import time

import numpy as np

from flies_and_goos.engine import (
    IS_FLY_ALL,
    N_CODONS,
    STABILITY_ALL,
    all_codon_texts,
    outcomes_against_all,
)

TIES_FROM_SIGNATURE_METHOD = 423580
"""Draw count independently derived by ``analysis/ties.py``; see cross-check."""


def sweep(matrix_path: str | None, progress_every: int = 5000):
    """Return (wins, losses, draws) arrays, optionally saving the beat matrix."""
    texts = all_codon_texts()
    wins = np.zeros(N_CODONS, dtype=np.int32)
    losses = np.zeros(N_CODONS, dtype=np.int32)

    packed_width = (N_CODONS + 7) // 8
    matrix = (
        np.empty((N_CODONS, packed_width), dtype=np.uint8)
        if matrix_path
        else None
    )

    started = time.monotonic()
    for index, text in enumerate(texts):
        outcomes = outcomes_against_all(text)
        beaten_by = outcomes < 0
        wins[index] = int(np.count_nonzero(outcomes > 0))
        losses[index] = int(np.count_nonzero(beaten_by))
        if matrix is not None:
            matrix[index] = np.packbits(beaten_by)

        if progress_every and (index + 1) % progress_every == 0:
            elapsed = time.monotonic() - started
            rate = (index + 1) / elapsed
            print(
                f"  {index + 1:>5}/{N_CODONS} codons "
                f"({elapsed:.0f}s elapsed, {(N_CODONS - index - 1) / rate:.0f}s left)",
                flush=True,
            )

    draws = N_CODONS - wins - losses
    if matrix is not None:
        np.save(matrix_path, matrix)
        print(f"beat matrix -> {matrix_path} ({matrix.nbytes / 1e6:.0f} MB)")
    return wins, losses, draws


def check(wins, losses, draws) -> None:
    """Consistency checks; these are the guard rails on the whole analysis."""
    total_wins = int(wins.sum())
    total_losses = int(losses.sum())
    assert total_wins == total_losses, (
        f"every win is someone's loss, got {total_wins} vs {total_losses}"
    )
    total_draws = int(draws.sum())
    assert total_draws == TIES_FROM_SIGNATURE_METHOD, (
        f"sweep found {total_draws} draws, signature method found "
        f"{TIES_FROM_SIGNATURE_METHOD}"
    )
    assert int((wins + losses + draws).max()) == N_CODONS


def write_tsv(path, texts, wins, losses, draws, order) -> None:
    with open(path, "w") as handle:
        handle.write("codon\ttype\tstability\twins\tlosses\tdraws\tp_win\n")
        for index in order:
            handle.write(
                f"{texts[index]}\t"
                f"{'Fly' if IS_FLY_ALL[index] else 'Goo'}\t"
                f"{STABILITY_ALL[index]}\t"
                f"{wins[index]}\t{losses[index]}\t{draws[index]}\t"
                f"{wins[index] / N_CODONS:.6f}\n"
            )


def show(title, indexes, texts, wins, losses, draws) -> None:
    print(f"\n{title}")
    print(f"  {'codon':6} {'type':5} {'stab':5} {'p(win)':8} {'p(loss)':8} draws")
    for index in indexes:
        print(
            f"  {texts[index]:6} "
            f"{'Fly' if IS_FLY_ALL[index] else 'Goo':5} "
            f"{STABILITY_ALL[index]:<5} "
            f"{wins[index] / N_CODONS:<8.4f} "
            f"{losses[index] / N_CODONS:<8.4f} "
            f"{draws[index]}"
        )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tsv", default="winrates.tsv")
    parser.add_argument("--matrix", default="beat_matrix.npy")
    parser.add_argument("--no-matrix", action="store_true")
    args = parser.parse_args()

    print(f"sweeping {N_CODONS} x {N_CODONS} matchups...")
    wins, losses, draws = sweep(None if args.no_matrix else args.matrix)
    check(wins, losses, draws)

    texts = all_codon_texts()
    order = np.lexsort((np.arange(N_CODONS), -wins))
    write_tsv(args.tsv, texts, wins, losses, draws, order)
    print(f"\nwin rates -> {args.tsv}")

    show("Top 20 by P(win):", order[:20], texts, wins, losses, draws)
    show("Bottom 10 by P(win):", order[-10:], texts, wins, losses, draws)

    print(
        f"\nP(win) mean {wins.mean() / N_CODONS:.4f}, "
        f"best {wins.max() / N_CODONS:.4f}, worst {wins.min() / N_CODONS:.4f}"
    )
    print(f"total draws {int(draws.sum())} (cross-check passed)")


if __name__ == "__main__":
    main()
