# Flies and Goos

A 2-player game: each player writes a 3-character codon (A-Z, 0-9), and the
codons face off over visual features of their glyphs in DM Mono. Full rules are
in `rules.docx`.

## Setup

The uv cache is kept inside this directory (the default `~/.cache/uv` is outside
the sandbox), so every uv command needs the `UV_CACHE_DIR` prefix:

```sh
UV_CACHE_DIR=.uv-cache uv sync --extra dev
UV_CACHE_DIR=.uv-cache uv run pytest
```

Python 3.13 from miniconda is used; uv cannot download its own interpreters
under the sandbox.

## Usage

```sh
UV_CACHE_DIR=.uv-cache uv run python -m flies_and_goos B4U N95
```

```python
from flies_and_goos import Codon, face_off

result = face_off("GR8", "3PO")
result.relationship        # 'Foe'
[r.contest for r in result.rounds]   # [7, 8, 9] - two tiebreakers
str(result.winner)         # 'GR8'

Codon("A1S").type          # 'Fly'
Codon("A1S").stability     # 6
```

`result.winner` is `None` if all 10 contests tie.

## Analysis

Exhaustive results over all 46,656 codons are written up in
[`report.md`](report.md). Headlines: non-identical codons that tie all 10
contests do exist (319,812 ordered pairs, and no two characters are
functionally identical, so every tie is earned); the best codon against a
uniform opponent is `BFN` at P(win) 0.6925; and P(win) depends only on a
codon's *multiset* of characters, so there are 8,436 distinct strategies rather
than 46,656.

The best partner for `BFN` is `BPN`, which defeats 85.8% of the codons that beat
`BFN`; only 4.37% of codons beat both. There is no meaningful ranking of codons
though — in 44.9% of matchups the lower-rated codon wins.

```sh
UV_CACHE_DIR=.uv-cache uv run python analysis/ties.py       # ~0.5s
UV_CACHE_DIR=.uv-cache uv run python analysis/winrates.py   # ~105s, writes beat_matrix.npy
UV_CACHE_DIR=.uv-cache uv run python analysis/partner.py    # ~30s, needs the above
```

## Layout

- `src/flies_and_goos/tables.py` — the per-character score tables transcribed
  from the reference tables in `rules.docx`. Each table is checked at import
  time to cover all 36 characters exactly once.
- `src/flies_and_goos/game.py` — codon type, stability, relationship, contest
  selection, and the tiebreaker loop.
- `src/flies_and_goos/__main__.py` — round-by-round CLI.
- `src/flies_and_goos/engine.py` — vectorized evaluator (one codon against all
  46,656 at once), cross-checked against `game.py` in the tests.
- `src/flies_and_goos/ties.py` — exact all-contest tie search.
- `analysis/` — drivers that produce the numbers in `report.md`.
