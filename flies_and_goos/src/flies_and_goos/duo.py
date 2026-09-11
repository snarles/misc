"""Two-codon analysis: how well one codon covers another's losses.

Built on the bit-packed beat-matrix written by ``analysis/winrates.py``. Its
orientation is the thing to keep straight:

    matrix[j] has bit X set  <=>  codon X defeats codon j

so row ``j`` lists everyone who beats ``j``, and *column* ``X`` lists everyone
``X`` beats. Summing a selection of rows therefore counts, for every candidate
``X`` at once, how many of those selected codons ``X`` defeats.
"""

import os

import numpy as np

from .engine import N_CODONS, outcomes_against_all

MATRIX_PATH = "beat_matrix.npy"
PACKED_WIDTH = (N_CODONS + 7) // 8


class BeatMatrixMissing(FileNotFoundError):
    pass


def load_beat_matrix(path: str = MATRIX_PATH) -> np.ndarray:
    """Memory-map the beat-matrix, or explain how to build it."""
    if not os.path.exists(path):
        raise BeatMatrixMissing(
            f"{path} not found - build it first with:\n"
            f"  UV_CACHE_DIR=.uv-cache uv run python analysis/winrates.py"
        )
    matrix = np.load(path, mmap_mode="r")
    if matrix.shape != (N_CODONS, PACKED_WIDTH):
        raise ValueError(f"{path} has unexpected shape {matrix.shape}")
    return matrix


def defeated_by(codon: str) -> np.ndarray:
    """Indices of the codons that defeat ``codon``."""
    return np.flatnonzero(outcomes_against_all(codon) < 0)


def defeats(codon: str) -> np.ndarray:
    """Indices of the codons that ``codon`` defeats."""
    return np.flatnonzero(outcomes_against_all(codon) > 0)


def coverage_counts(
    targets: np.ndarray, matrix: np.ndarray, chunk: int = 2048
) -> np.ndarray:
    """For every codon X, how many of ``targets`` does X defeat?

    Returns an int32 array of length ``N_CODONS``. Chunked because unpacking all
    of ``targets`` at once would be ~800 MB for a typical target set.
    """
    counts = np.zeros(N_CODONS, dtype=np.int32)
    for start in range(0, len(targets), chunk):
        rows = np.asarray(matrix[targets[start : start + chunk]])
        counts += np.unpackbits(rows, axis=1)[:, :N_CODONS].sum(
            axis=0, dtype=np.int32
        )
    return counts


def coverage_by_sweep(targets: np.ndarray, candidates) -> np.ndarray:
    """Same counts as :func:`coverage_counts`, derived independently by
    replaying matchups with the engine. Slow; used to verify the matrix path.
    """
    mask = np.zeros(N_CODONS, dtype=bool)
    mask[targets] = True
    return np.array(
        [
            int(np.count_nonzero((outcomes_against_all(text) > 0) & mask))
            for text in candidates
        ],
        dtype=np.int32,
    )
