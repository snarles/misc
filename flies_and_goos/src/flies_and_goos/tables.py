"""Per-character reference tables, transcribed from the tables in ``rules.docx``.

Every table must assign a score to each of the 36 codon characters exactly once;
``_table`` enforces that at import time, which is the cheapest guard against a
transcription slip.
"""

ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

CURVED = frozenset("0235689BCDGJOPQRSU")
STRAIGHT = frozenset("147AEFHIKLMNTVWXYZ")


def _table(name: str, spec: list[tuple[int, str]]) -> dict[str, int]:
    table: dict[str, int] = {}
    for score, chars in spec:
        for ch in chars:
            if ch not in ALPHABET:
                raise ValueError(f"{name}: {ch!r} is not a codon character")
            if ch in table:
                raise ValueError(f"{name}: {ch!r} listed twice")
            table[ch] = score
    missing = set(ALPHABET) - table.keys()
    if missing:
        raise ValueError(f"{name}: no score for {''.join(sorted(missing))}")
    return table


if CURVED | STRAIGHT != set(ALPHABET) or CURVED & STRAIGHT:
    raise ValueError("CURVED and STRAIGHT must partition the alphabet")


STABILITY = _table(
    "stability",
    [
        (0, "47FPQTVY"),
        (1, "035689CJOSU"),
        (2, "AGHKMNRWX"),
        (3, "12BDEILZ"),
    ],
)

ORTHOGONAL_LINES = _table(
    "orthogonal lines",
    [
        (0, "03689COQSVWX"),
        (1, "257AJKY"),
        (2, "14GLMNTUZ"),
        (3, "DFHIPR"),
        (4, "BE"),
    ],
)

V_JUNCTIONS = _table(
    "V-junctions",
    [
        (0, "0689CGHIJKOQSTUXY"),
        (1, "1237AFLPRV"),
        (2, "45BDENZ"),
        (3, "MW"),
    ],
)

TOP_BOTTOM_BUMPS = _table(
    "top and bottom bumps",
    [
        (0, "7BDEFHIKLPRTXYZ"),
        (1, "1245AJUVQ"),
        (2, "03689CGMNOS"),
        (3, "W"),
    ],
)

JUNCTIONS = _table(
    "junctions",
    [
        (0, "CJOSU"),
        (1, "236789GKLQTVXY"),
        (2, "015DFHINPZ"),
        (3, "4AEMRW"),
        (4, "B"),
    ],
)

UPSIDE_DOWN_STABILITY = _table(
    "upside-down stability",
    [
        (0, "14AJL"),
        (1, "023689CGOQS"),
        (2, "HKMNUVXY"),
        (3, "57BDEFIPRTWZ"),
    ],
)

#: T-junctions plus loops tangent to the floor. The loop term is what
#: distinguishes 0/6/8/B/D/O - characters whose enclosed space rests on the
#: floor - from 9/P/Q/R/A, whose loops sit clear of it.
T_JUNCTIONS_AND_FLOOR_LOOPS = _table(
    "T-junctions plus floor-tangent loops",
    [
        (0, "23457CGJKLMNSUVWXYZ"),
        (1, "189DEFOPQT"),
        (2, "6ABHIR"),
        (3, "0"),
    ],
)

LEFT_DESCENDING_SLANTS = _table(
    "left-descending slanted line segments",
    [
        (0, "3689BCDEFGHIJLNOPQRSTU"),
        (1, "012457AKMVXYZ"),
        (2, "W"),
    ],
)

SIDE_BUMPS = _table(
    "side bumps",
    [
        (0, "1AEFHIJKLMNTUVWXYZ"),
        (1, "247CDGPR"),
        (2, "03569BOQS"),
        (4, "8"),
    ],
)

VALLEYS = _table(
    "valleys",
    [
        (0, "127EFILTZ"),
        (1, "34ACDGHJKMNOPQRSUVXY"),
        (2, "05689BW"),
    ],
)

#: Contest #0 through #9, indexed by contest number.
CONTESTS: tuple[tuple[str, dict[str, int]], ...] = (
    ("stability", STABILITY),
    ("number of orthogonal line segments", ORTHOGONAL_LINES),
    ("number of V-junctions", V_JUNCTIONS),
    ("number of top and bottom bumps", TOP_BOTTOM_BUMPS),
    ("number of junctions", JUNCTIONS),
    ("upside-down stability", UPSIDE_DOWN_STABILITY),
    ("number of T-junctions plus loops tangent to the floor",
     T_JUNCTIONS_AND_FLOOR_LOOPS),
    ("number of left-descending slanted line segments", LEFT_DESCENDING_SLANTS),
    ("number of side bumps", SIDE_BUMPS),
    ("number of valleys", VALLEYS),
)

NUM_CONTESTS = len(CONTESTS)
