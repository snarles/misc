"""
test_wc_rules.py

Verify that wc_rules.winner (rule-based, derived from readme.md)
matches the lookup-table-based winner used in elimination_codon.py /
elimination_wc.py on a diverse set of word pairs.

We re-implement the lookup-table winner inline rather than importing it,
because importing elimination_codon.py would launch its interactive
game loop.
"""

import pandas as pd
import numpy as np
from numpy.random import default_rng

import wc_rules


# --------------------------------------------------------------------------
# Lookup-table reference implementation (copied verbatim from
# elimination_codon.py, minus the interactive loop).
# --------------------------------------------------------------------------

table  = pd.read_csv("letters_wc_beats.txt")
tablr  = pd.read_csv("letters_wc_reverses.txt")
loses_to    = {table.loc[i, "letter"]: table.loc[i, "beats"]    for i in range(len(table))}
reverses_to = {tablr.loc[i, "letter"]: tablr.loc[i, "reverses"] for i in range(len(tablr))}


def lookup_wrap(w_short, w_long):
    w = w_short
    while len(w) < len(w_long):
        w += w_short
    return w[:len(w_long)]


def lookup_winner_wo_reverse(w1, w2):
    assert len(w1) == len(w2)
    won_yet = False
    w1_first = 0
    w1_adv = 0
    for i in range(len(w2)):
        l1 = w1[i]
        l2 = w2[i]
        if l1 != l2:
            if l2 in loses_to[l1]:
                w1_adv += 1
                if not won_yet:
                    won_yet = True
                    w1_first = 1
            else:
                w1_adv -= 1
                if not won_yet:
                    won_yet = True
                    w1_first = -1
    if w1_adv == 0:
        return w1_first
    return int(np.sign(w1_adv))


def lookup_winner(w1, w2):
    w1 = w1.replace(" ", "")
    w2 = w2.replace(" ", "")
    if len(w1) > len(w2):
        return -lookup_winner(w2, w1)
    w1 = lookup_wrap(w1, w2)
    n_ties = sum(1 for i in range(len(w2)) if w2[i] in reverses_to[w1[i]])
    base = lookup_winner_wo_reverse(w1, w2)
    return base if n_ties % 2 == 0 else -base


# --------------------------------------------------------------------------
# Test-case generation
# --------------------------------------------------------------------------

def random_word(rng, alphabet, length):
    return "".join(rng.choice(list(alphabet), size=length))


def build_test_cases(rng):
    cases = []

    # 1. The four worked examples from readme.md.
    cases += [("GOD", "SIN"), ("F35", "DR1"), ("MCE", "MEN"), ("0AB", "94X")]

    full_alphabet = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
    letters_only  = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    digits_only   = list("0123456789")

    # 2. Equal-length mixed-alphabet pairs across several lengths.
    for length in range(1, 7):
        for _ in range(8):
            cases.append((random_word(rng, full_alphabet, length),
                          random_word(rng, full_alphabet, length)))

    # 3. Unequal-length pairs (exercises Step 1 wrapping).
    for _ in range(20):
        l1 = int(rng.integers(1, 7))
        l2 = int(rng.integers(1, 7))
        if l1 == l2:
            l2 += 1
        cases.append((random_word(rng, full_alphabet, l1),
                      random_word(rng, full_alphabet, l2)))

    # 4. Letters-only pairs (forces the Sound Classification path).
    for _ in range(15):
        L = int(rng.integers(2, 6))
        cases.append((random_word(rng, letters_only, L),
                      random_word(rng, letters_only, L)))

    # 5. Digits-only pairs (forces digit-vs-digit Shape + outranking).
    for _ in range(15):
        L = int(rng.integers(2, 6))
        cases.append((random_word(rng, digits_only, L),
                      random_word(rng, digits_only, L)))

    # 6. Single-character pairs covering every digit-to-letter conversion
    #    (exercises the letter-vs-digit proximity branch directly).
    for d, ell in wc_rules.DIGIT_AS_LETTER.items():
        cases.append((d, ell))
        cases.append((ell, d))

    return cases


def main():
    rng = default_rng(42)
    cases = build_test_cases(rng)
    assert len(cases) >= 100, "expected >= 100 test cases, got %d" % len(cases)

    mismatches = []
    for w1, w2 in cases:
        a = wc_rules.winner(w1, w2)
        b = lookup_winner(w1, w2)
        if a != b:
            mismatches.append((w1, w2, a, b))

    print("Tested %d cases." % len(cases))
    if mismatches:
        print("MISMATCHES (%d):" % len(mismatches))
        for w1, w2, a, b in mismatches[:20]:
            print("  %s vs %s : rules=%d  lookup=%d" % (w1, w2, a, b))
        raise SystemExit(1)
    print("All cases match.")


if __name__ == "__main__":
    main()
