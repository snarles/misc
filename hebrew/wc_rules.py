"""
wc_rules.py

Direct, rule-based implementation of the Word Chaos combat system
described in readme.md.

This module re-implements the per-character proximity (reversal) and
comparison (beats) rules from first principles, *without* consulting the
precomputed lookup tables (letters_wc_beats.txt, letters_wc_reverses.txt)
used by elimination_codon.py / elimination_wc.py.

Public API (mirrors the combat-system functions in elimination_codon.py):
    wrap(short, long)            - Step 1: extend short word to long's length.
    is_proximal(c1, c2)          - Step 3: per-pair reversal (proximity) check.
    pair_winner(c1, c2)          - Step 5: per-pair comparison; +1/-1/0.
    winner_wo_reverse(w1, w2)    - Step 4 + Step 5: conventional outcome.
    winner(w1, w2)               - Steps 1-6: full combat; +1 if w1, -1 if w2, 0 tie.
"""

import string


# --------------------------------------------------------------------------
# Step 5 -- Classification systems
# --------------------------------------------------------------------------

# Shape Classification (used when at least one character in the pair is a digit)
SHAPE_STRAIGHT = set("147AEFHIKLMNTVWXYZ")
SHAPE_CURVED   = set("235CGJSU")
SHAPE_LOOPY    = set("0689BDOPQR")
# Cyclic hierarchy:  Straight > Loopy > Curved > Straight
SHAPE_BEATS = {"straight": "loopy", "loopy": "curved", "curved": "straight"}

# Sound Classification (letters only)
SOUND_VOWELS = set("AEIOUY")
SOUND_HARD   = set("BCDGKPQT")
SOUND_SOFT   = set("FHJLMNRSVWXZ")
# Cyclic hierarchy:  Vowels > Hard > Soft > Vowels
SOUND_BEATS = {"vowel": "hard", "hard": "soft", "soft": "vowel"}

# Step 3 -- digit-to-letter conversion table for letter-vs-digit proximity.
DIGIT_AS_LETTER = {
    "0": "O", "1": "I", "2": "Z", "3": "E", "4": "A",
    "5": "S", "6": "G", "7": "T", "8": "B", "9": "J",
}

ALPHABET = string.ascii_uppercase  # "A".."Z"


def shape_class(c):
    if c in SHAPE_STRAIGHT: return "straight"
    if c in SHAPE_CURVED:   return "curved"
    if c in SHAPE_LOOPY:    return "loopy"
    raise ValueError("unknown character for shape classification: %r" % c)


def sound_class(c):
    if c in SOUND_VOWELS: return "vowel"
    if c in SOUND_HARD:   return "hard"
    if c in SOUND_SOFT:   return "soft"
    raise ValueError("not a sound-classifiable letter: %r" % c)


# --------------------------------------------------------------------------
# Step 3 -- Proximity / reversal check
# --------------------------------------------------------------------------

def _letters_proximal(a, b):
    """True iff letter `a` is the same as `b`, or within four letters before
    or after `b` in the alphabet (A<->Z wraparound)."""
    ia = ALPHABET.index(a)
    ib = ALPHABET.index(b)
    diff = (ia - ib) % 26     # 0..25, cyclic forward distance
    return diff <= 4 or diff >= 22   # within +/-4 mod 26


def _digits_proximal(a, b):
    """True iff digit `a` equals `b`, `b`+1, or `b`-1 (mod 10, so 0 follows 9)."""
    diff = (int(a) - int(b)) % 10
    return diff <= 1 or diff >= 9


def is_proximal(c1, c2):
    """Step 3: returns True if the (c1, c2) pair triggers a reversal."""
    d1, d2 = c1.isdigit(), c2.isdigit()
    if not d1 and not d2:
        return _letters_proximal(c1, c2)
    if d1 and d2:
        return _digits_proximal(c1, c2)
    # Letter-vs-digit: convert the digit to its letter form, then use the
    # letter-vs-letter rule.
    if d1:
        return _letters_proximal(DIGIT_AS_LETTER[c1], c2)
    else:
        return _letters_proximal(c1, DIGIT_AS_LETTER[c2])


# --------------------------------------------------------------------------
# Step 5 -- Per-pair character comparison
# --------------------------------------------------------------------------

def pair_winner(c1, c2):
    """Returns +1 if c1 wins the pair, -1 if c2 wins, 0 if tied (identical)."""
    if c1 == c2:
        return 0

    use_shape = c1.isdigit() or c2.isdigit()
    if use_shape:
        cls1, cls2 = shape_class(c1), shape_class(c2)
        beats = SHAPE_BEATS
    else:
        cls1, cls2 = sound_class(c1), sound_class(c2)
        beats = SOUND_BEATS

    if cls1 != cls2:
        # Outclassing
        return 1 if beats[cls1] == cls2 else -1

    # Same class -> outranking
    d1, d2 = c1.isdigit(), c2.isdigit()
    if d1 != d2:
        # Letters always outrank digits.
        return -1 if d1 else 1
    if not d1:
        # Both letters, same class -> later in the alphabet wins.
        return 1 if c1 > c2 else -1
    # Both digits, same class -> higher digit wins.
    return 1 if int(c1) > int(c2) else -1


# --------------------------------------------------------------------------
# Step 1 -- Equalize word lengths
# --------------------------------------------------------------------------

def wrap(w_short, w_long):
    """Extend w_short by repeating it, then truncate to len(w_long)."""
    w = w_short
    while len(w) < len(w_long):
        w += w_short
    return w[:len(w_long)]


# --------------------------------------------------------------------------
# Step 4 -- Conventional outcome (no reversals applied)
# --------------------------------------------------------------------------

def winner_wo_reverse(w1, w2):
    """Conventional winner: +1, -1, or 0.

    Counts pair wins; if both sides win the same number of pairs, the
    winner of the first non-tied pair breaks the tie (per readme Step 4)."""
    assert len(w1) == len(w2)
    first = 0   # +/-1 of the first non-tied pair, 0 if all tied
    adv = 0     # (w1 wins) - (w2 wins)
    for c1, c2 in zip(w1, w2):
        r = pair_winner(c1, c2)
        if r != 0:
            adv += r
            if first == 0:
                first = r
    if adv == 0:
        return first
    return 1 if adv > 0 else -1


# --------------------------------------------------------------------------
# Step 6 -- Combine reversals with the conventional outcome
# --------------------------------------------------------------------------

def winner(w1, w2):
    """Full combat. Returns +1 if w1 wins, -1 if w2 wins, 0 for a tie."""
    w1 = w1.replace(" ", "")
    w2 = w2.replace(" ", "")
    if len(w1) > len(w2):
        return -winner(w2, w1)
    w1 = wrap(w1, w2)
    n_rev = sum(1 for a, b in zip(w1, w2) if is_proximal(a, b))
    base = winner_wo_reverse(w1, w2)
    return base if n_rev % 2 == 0 else -base
