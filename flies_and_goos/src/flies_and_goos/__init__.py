"""Flies and Goos: a 2-player codon game.

Rules live in ``rules.docx`` at the repo root.
"""

from .game import (
    FLY,
    FOE,
    FRIEND,
    GOO,
    Codon,
    FaceOff,
    Round,
    face_off,
    opening_contest,
    play_round,
    relationship,
)

__version__ = "0.1.0"

__all__ = [
    "Codon",
    "FaceOff",
    "Round",
    "FLY",
    "GOO",
    "FRIEND",
    "FOE",
    "face_off",
    "opening_contest",
    "play_round",
    "relationship",
]
