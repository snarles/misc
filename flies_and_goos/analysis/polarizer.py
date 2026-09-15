"""Task 5: do strong 3-fortresses divide labour between a polarizer and two mop-ups?

The hypothesis: one member is a "polarizer" with a lopsided win rate against one
of the two codon types, and the other two cover the type it leaves open. Part 4
found the mechanism in one instance - JJJ beats 89.1% of Flies and 4.3% of Goos -
and this asks whether that is the general shape of a strong fortress.

This is correlational. It can show the structure is present in fortresses the
search found; it cannot show the structure is what makes them strong.

Run: UV_CACHE_DIR=.uv-cache uv run python analysis/polarizer.py [--csv *.csv]
"""

import argparse
import csv
import sys
from functools import lru_cache
from itertools import permutations

import numpy as np

from flies_and_goos.duo import BeatMatrixMissing, coverage_counts, load_beat_matrix
from flies_and_goos.engine import (
    IS_FLY_ALL,
    N_CODONS,
    codon_index,
    codon_text,
    outcomes_against_all,
)
from flies_and_goos.game import FLY, GOO, Codon

FLY_INDEX = np.flatnonzero(IS_FLY_ALL)
GOO_INDEX = np.flatnonzero(~IS_FLY_ALL)
N_FLIES = len(FLY_INDEX)
N_GOOS = len(GOO_INDEX)

#: JJJ's split, published in report.md part 4. Asserted at run time because a
#: swapped Fly/Goo mask would reverse every conclusion below without erroring.
JJJ_BEATS_FLIES = 20786
JJJ_BEATS_GOOS = 1006

VERIFY_SAMPLE = 12

#: Bands the results are broken down by, as (label, predicate on totalizers).
BANDS = (
    ("perfect (0)", lambda n: n == 0),
    ("<= 30", lambda n: n <= 30),
    ("31-120", lambda n: 30 < n <= 120),
    ("> 120", lambda n: n > 120),
)


def polarity_table(matrix):
    """``wF``, ``wG`` for every codon: the fraction of each type it defeats."""
    wins_fly = coverage_counts(FLY_INDEX, matrix)
    wins_goo = coverage_counts(GOO_INDEX, matrix)
    return wins_fly / N_FLIES, wins_goo / N_GOOS


@lru_cache(maxsize=None)
def beaten_mask(text):
    """Boolean mask of the codons ``text`` defeats. Memoised - members repeat."""
    return outcomes_against_all(text) > 0


def route(member: str, covered: str) -> str:
    """How a member covers a type: Foe means higher score wins, Friend lower."""
    return "Foe-high" if Codon(member).type == covered else "Friend-low"


def orbit(members):
    """Canonical form under simultaneous position permutation."""
    return min(
        tuple(sorted("".join(t[i] for i in order) for t in members))
        for order in permutations(range(3))
    )


def profile(members, wf, wg):
    """Polarizer/mop-up structure of one fortress."""
    idx = [codon_index(m) for m in members]
    polarity = [float(wf[i] - wg[i]) for i in idx]
    pole = max(range(3), key=lambda k: abs(polarity[k]))

    covered = FLY if polarity[pole] > 0 else GOO
    other = GOO if covered == FLY else FLY
    other_index = GOO_INDEX if other == GOO else FLY_INDEX

    # Combined cover of the type the polarizer leaves open, as a union over the
    # two mop-ups rather than the better of them.
    mops = [members[k] for k in range(3) if k != pole]
    union = beaten_mask(mops[0]) | beaten_mask(mops[1])
    mop_cover = float(union[other_index].mean())

    pole_cover = float(wf[idx[pole]] if covered == FLY else wg[idx[pole]])

    # Which opponents defeat all three, split by type.
    beats_all = np.ones(N_CODONS, dtype=bool)
    for m in members:
        beats_all &= outcomes_against_all(m) < 0

    return {
        "polarizer": members[pole],
        "abs_polarity": abs(polarity[pole]),
        "covered": covered,
        "pole_cover": pole_cover,
        "mop_cover": mop_cover,
        "pole_route": route(members[pole], covered),
        "mop_routes": tuple(sorted(route(m, other) for m in mops)),
        "tot_fly": int(beats_all[FLY_INDEX].sum()),
        "tot_goo": int(beats_all[GOO_INDEX].sum()),
        "members": tuple(members),
    }


def show(title, rows, headers):
    print(f"\n{title}")
    print("  " + "  ".join(f"{h:>13}" for h in headers))
    for row in rows:
        cells = [f"{c:>13.3f}" if isinstance(c, float) else f"{str(c):>13}"
                 for c in row]
        print("  " + "  ".join(cells))


def load_rows(paths):
    rows = []
    for path in paths:
        try:
            rows.extend(csv.DictReader(open(path)))
        except OSError as error:
            sys.exit(f"cannot read {path}: {error}")
    if not rows:
        sys.exit("no climbs loaded")
    return rows


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--csv", nargs="+", default=["fortresses.csv"])
    parser.add_argument("--top", type=int, default=12)
    args = parser.parse_args()

    try:
        matrix = load_beat_matrix()
    except BeatMatrixMissing as error:
        sys.exit(str(error))

    wf, wg = polarity_table(matrix)

    # --- verification -----------------------------------------------------
    jjj = codon_index("JJJ")
    assert round(wf[jjj] * N_FLIES) == JJJ_BEATS_FLIES, (
        f"JJJ beats {round(wf[jjj] * N_FLIES)} Flies, expected {JJJ_BEATS_FLIES}"
        " - Fly/Goo mask may be inverted"
    )
    assert round(wg[jjj] * N_GOOS) == JJJ_BEATS_GOOS, (
        f"JJJ beats {round(wg[jjj] * N_GOOS)} Goos, expected {JJJ_BEATS_GOOS}"
    )
    rng = np.random.default_rng(20260915)
    for index in rng.choice(N_CODONS, size=VERIFY_SAMPLE, replace=False):
        outcomes = outcomes_against_all(codon_text(int(index)))
        won = outcomes > 0
        assert round(wf[index] * N_FLIES) == int(won[FLY_INDEX].sum()), (
            f"matrix and engine disagree on Flies for {codon_text(int(index))}"
        )
        assert round(wg[index] * N_GOOS) == int(won[GOO_INDEX].sum()), (
            f"matrix and engine disagree on Goos for {codon_text(int(index))}"
        )
    print(
        f"verified: JJJ splits {JJJ_BEATS_FLIES}/{N_FLIES} Flies and "
        f"{JJJ_BEATS_GOOS}/{N_GOOS} Goos as report.md part 4 states; "
        f"{VERIFY_SAMPLE} codons re-derived by replaying matchups"
    )

    polarity_all = np.abs(wf - wg)
    print(
        f"\nbaseline over all {N_CODONS} codons: mean |polarity| "
        f"{polarity_all.mean():.3f}, median {np.median(polarity_all):.3f}, "
        f"max {polarity_all.max():.3f}"
    )

    rows = load_rows(args.csv)
    finals = {}
    seeds = []
    for row in rows:
        members = (row["final_a"], row["final_b"], row["final_c"])
        finals.setdefault(orbit(members), (members, int(row["final_totalizers"])))
        seeds.append((row["initial_a"], row["initial_b"], row["initial_c"]))
    print(f"{len(rows)} climbs -> {len(finals)} distinct fortresses")

    profiles = [
        (totalizers, profile(list(members), wf, wg))
        for members, totalizers in finals.values()
    ]
    for totalizers, p in profiles:
        assert p["tot_fly"] + p["tot_goo"] == totalizers, (
            f"type split {p['tot_fly']}+{p['tot_goo']} != {totalizers}"
        )

    control = [profile(list(s), wf, wg) for s in seeds[: min(len(seeds), 200)]]

    # --- structure by strength band ---------------------------------------
    band_rows = []
    for label, keep in BANDS:
        sel = [p for n, p in profiles if keep(n)]
        if not sel:
            continue
        band_rows.append((
            label,
            len(sel),
            float(np.mean([p["abs_polarity"] for p in sel])),
            float(np.mean([p["pole_cover"] for p in sel])),
            float(np.mean([p["mop_cover"] for p in sel])),
        ))
    band_rows.append((
        "random seeds",
        len(control),
        float(np.mean([p["abs_polarity"] for p in control])),
        float(np.mean([p["pole_cover"] for p in control])),
        float(np.mean([p["mop_cover"] for p in control])),
    ))
    show(
        "Polarizer structure by strength band (means):",
        band_rows,
        ("band", "n", "|polarity|", "pole cover", "mop cover"),
    )

    # --- is the labour actually divided? ----------------------------------
    print("\nWhere the surviving totalizers sit, by band:")
    print(f"  {'band':>13}  {'n':>4}  {'polarizer type':>15}  {'other type':>11}")
    for label, keep in BANDS:
        sel = [(n, p) for n, p in profiles if keep(n)]
        if not sel:
            continue
        pole_side = sum(
            p["tot_fly"] if p["covered"] == FLY else p["tot_goo"] for _, p in sel
        )
        other_side = sum(
            p["tot_goo"] if p["covered"] == FLY else p["tot_fly"] for _, p in sel
        )
        print(f"  {label:>13}  {len(sel):>4}  {pole_side:>15}  {other_side:>11}")

    # --- routes ------------------------------------------------------------
    print("\nWhich type the polarizer covers:")
    for label, keep in BANDS:
        sel = [p for n, p in profiles if keep(n)]
        if not sel:
            continue
        fly = sum(1 for p in sel if p["covered"] == FLY)
        print(f"  {label:>13}: Fly {fly}/{len(sel)}, Goo {len(sel) - fly}/{len(sel)}")

    print("\nHow the polarizer covers its type:")
    for label, keep in BANDS:
        sel = [p for n, p in profiles if keep(n)]
        if not sel:
            continue
        friend = sum(1 for p in sel if p["pole_route"] == "Friend-low")
        print(f"  {label:>13}: Friend-low {friend}/{len(sel)}, "
              f"Foe-high {len(sel) - friend}/{len(sel)}")

    print("\nHow the two mop-ups cover the other type:")
    for label, keep in BANDS:
        sel = [p for n, p in profiles if keep(n)]
        if not sel:
            continue
        combos = {}
        for p in sel:
            combos[p["mop_routes"]] = combos.get(p["mop_routes"], 0) + 1
        pretty = ", ".join(f"{'+'.join(k)}: {v}" for k, v in
                           sorted(combos.items(), key=lambda kv: -kv[1]))
        print(f"  {label:>13}: {pretty}")

    # --- the strongest fortresses in detail --------------------------------
    best = sorted(profiles, key=lambda pair: pair[0])[: args.top]
    print(f"\n{'fortress':>16}  {'tot':>4}  {'polarizer':>10}  {'covers':>6}  "
          f"{'|pol|':>6}  {'pole':>6}  {'mops':>6}  route")
    for totalizers, p in best:
        print(f"  {' '.join(p['members']):>14}  {totalizers:>4}  {p['polarizer']:>10}  "
              f"{p['covered']:>6}  {p['abs_polarity']:>6.3f}  "
              f"{p['pole_cover']:>6.3f}  {p['mop_cover']:>6.3f}  {p['pole_route']}")


if __name__ == "__main__":
    main()
