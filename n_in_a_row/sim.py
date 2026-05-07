"""Luck-only n-in-a-row simulation on a 4x4 board.

State = (s1, s2): two strings of A-P letters (sorted) giving the cells held by
player 1 and player 2 respectively. Cell labels:

    A B C D
    E F G H
    I J K L
    M N O P

Each turn the active player draws a uniformly random cell out of all 16:
  - empty       -> place own token there
  - own token   -> no-op
  - opponent    -> remove opponent's token (cell becomes empty)
First player to get 3 collinear tokens wins.
"""
import csv
import random
from collections import namedtuple


LETTERS = "ABCDEFGHIJKLMNOP"
LETTER_TO_RC = {L: (i // 4, i % 4) for i, L in enumerate(LETTERS)}
RC_TO_LETTER = {rc: L for L, rc in LETTER_TO_RC.items()}


def _build_win_lines():
    triples = []
    for r in range(4):
        for c in range(2):
            triples.append([(r, c + i) for i in range(3)])
    for c in range(4):
        for r in range(2):
            triples.append([(r + i, c) for i in range(3)])
    for r in range(2):
        for c in range(2):
            triples.append([(r + i, c + i) for i in range(3)])
    for r in range(2):
        for c in range(2, 4):
            triples.append([(r + i, c - i) for i in range(3)])
    return [frozenset(RC_TO_LETTER[rc] for rc in tri) for tri in triples]


WIN_LINES = _build_win_lines()


def normalize(state):
    s1, s2 = state
    return ("".join(sorted(s1)), "".join(sorted(s2)))


def is_valid(state):
    s1, s2 = state
    for s in (s1, s2):
        if any(c not in LETTERS for c in s):
            return False
        if len(s) != len(set(s)):
            return False
    if set(s1) & set(s2):
        return False
    return len(s1) + len(s2) <= 16


def _player_won(letters):
    s = set(letters)
    return any(line <= s for line in WIN_LINES)


def winner_of(state):
    s1, s2 = state
    w1, w2 = _player_won(s1), _player_won(s2)
    if w1 and w2:
        return None
    if w1:
        return 1
    if w2:
        return 2
    return None


def apply_move(state, mover, cell):
    s1, s2 = state
    own, opp = (s1, s2) if mover == 1 else (s2, s1)
    if cell in own:
        new_own, new_opp = own, opp
    elif cell in opp:
        new_own, new_opp = own, opp.replace(cell, "")
    else:
        new_own = "".join(sorted(own + cell))
        new_opp = opp
    return (new_own, new_opp) if mover == 1 else (new_opp, new_own)


def simulate(state, next_player, max_turns=10_000, rng=None, record_history=False):
    rng = rng or random
    state = normalize(state)
    history = [state] if record_history else None
    w = winner_of(state)
    if w is not None:
        return (w, 0, history) if record_history else (w, 0)
    mover = next_player
    for t in range(1, max_turns + 1):
        cell = rng.choice(LETTERS)
        state = apply_move(state, mover, cell)
        if record_history:
            history.append(state)
        if _player_won(state[mover - 1]):
            return (mover, t, history) if record_history else (mover, t)
        mover = 3 - mover
    return (None, max_turns, history) if record_history else (None, max_turns)


MCResult = namedtuple(
    "MCResult",
    "p1_wins p2_wins no_winner n p1_prob p2_prob no_winner_prob",
)


def monte_carlo(state, next_player, n_runs, max_turns=10_000, seed=None):
    rng = random.Random(seed)
    p1 = p2 = nw = 0
    for _ in range(n_runs):
        w, _ = simulate(state, next_player, max_turns, rng)
        if w == 1:
            p1 += 1
        elif w == 2:
            p2 += 1
        else:
            nw += 1
    n = n_runs
    return MCResult(p1, p2, nw, n, p1 / n, p2 / n, nw / n)


def line_features(state, player):
    own = set(state[player - 1])
    opp = set(state[2 - player])
    M = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    for line in WIN_LINES:
        a = sum(1 for c in line if c in own)
        b = sum(1 for c in line if c in opp)
        if a < 3 and b < 3:
            M[a][b] += 1
    return M


ML_FEATURE_INDICES = [(0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1)]
ML_FEATURE_NAMES = [
    "m01", "m02", "m10", "m11", "m12", "m20", "m21",
    "n_to_move", "n_opp", "imm_win_to_move", "imm_win_opp",
]


def immediate_win_cells(state, player):
    own = set(state[player - 1])
    opp = set(state[2 - player])
    cells = set()
    for line in WIN_LINES:
        if len(line & own) == 2 and len(line & opp) == 0:
            cells |= (line - own)
    return cells


def ml_features(state, to_move):
    M = line_features(state, to_move)
    feats = [M[a][b] for (a, b) in ML_FEATURE_INDICES]
    n_to_move = len(state[to_move - 1])
    n_opp = len(state[2 - to_move])
    feats.append(n_to_move)
    feats.append(n_opp)
    feats.append(len(immediate_win_cells(state, to_move)))
    feats.append(len(immediate_win_cells(state, 3 - to_move)))
    return feats


def visualize(state):
    s1, s2 = state
    cells = []
    for L in LETTERS:
        if L in s1:
            cells.append("X")
        elif L in s2:
            cells.append("O")
        else:
            cells.append(".")
    return "\n".join(" ".join(cells[r * 4:(r + 1) * 4]) for r in range(4))


def _fmt_matrix_row(row):
    return "[" + " ".join(f"{x:2d}" for x in row) + "]"


def render_turn(state, turn_num, show_features=False):
    if turn_num == 0:
        header = "Turn 0 — P1 to move"
    else:
        mover_just = 1 if turn_num % 2 == 1 else 2
        next_player = 3 - mover_just
        header = f"Turn {turn_num} (P{mover_just}) — P{next_player} to move"
    board_lines = visualize(state).split("\n")
    if not show_features:
        return "\n".join([header] + ["  " + L for L in board_lines])
    M1 = line_features(state, 1)
    M2 = line_features(state, 2)
    p1 = [_fmt_matrix_row(r) for r in M1]
    p2 = [_fmt_matrix_row(r) for r in M2]
    blank = " " * len(p1[0])
    out = [header]
    out.append(f"  {board_lines[0]}    P1: {p1[0]}    P2: {p2[0]}")
    out.append(f"  {board_lines[1]}        {p1[1]}        {p2[1]}")
    out.append(f"  {board_lines[2]}        {p1[2]}        {p2[2]}")
    out.append(f"  {board_lines[3]}        {blank}        {blank}")
    return "\n".join(out)


def monte_carlo_batch_to_csv(
    starts,
    next_player,
    n_runs,
    path,
    max_turns=10_000,
    seed=None,
    include_features=False,
):
    base_cols = [
        "s1", "s2", "next_player", "n",
        "p1_wins", "p2_wins", "no_winner",
        "p1_prob", "p2_prob", "no_winner_prob",
    ]
    feat_cols = [f"f{a}{b}" for a in range(3) for b in range(3)]
    header = base_cols + (feat_cols if include_features else [])
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(header)
        for i, start in enumerate(starts):
            run_seed = None if seed is None else seed + i
            res = monte_carlo(start, next_player, n_runs, max_turns, run_seed)
            s1, s2 = normalize(start)
            row = [
                s1, s2, next_player, res.n,
                res.p1_wins, res.p2_wins, res.no_winner,
                f"{res.p1_prob:.6f}", f"{res.p2_prob:.6f}", f"{res.no_winner_prob:.6f}",
            ]
            if include_features:
                M = line_features((s1, s2), next_player)
                row += [M[a][b] for a in range(3) for b in range(3)]
            w.writerow(row)


if __name__ == "__main__":
    assert len(WIN_LINES) == 24
    assert winner_of(("ABC", "")) == 1
    assert winner_of(("AFK", "")) == 1
    assert winner_of(("DGJ", "")) == 1
    assert winner_of(("ABE", "")) is None
    assert apply_move(("F", "J"), 1, "J") == ("F", "")
    assert apply_move(("F", "J"), 1, "F") == ("F", "J")
    assert apply_move(("F", "J"), 1, "A") == ("AF", "J")

    M = line_features(("", ""), 1)
    assert M[0][0] == 24 and sum(sum(r) for r in M) == 24

    M = line_features(("A", "B"), 1)
    assert M[0][0] == 18
    assert M[1][0] == 2
    assert M[0][1] == 3
    assert M[1][1] == 1
    assert sum(sum(r) for r in M) == 24
    assert M[2][2] == 0

    assert ml_features(("ABE", ""), 1) == [0, 0, 7, 0, 0, 2, 0, 3, 0, 2, 0]
    assert ml_features(("", ""), 1) == [0] * 11
    v1 = ml_features(("AB", "C"), 1)
    v2 = ml_features(("AB", "C"), 2)
    assert v1[7] == v2[8] and v1[8] == v2[7]
    assert v1[9] == v2[10] and v1[10] == v2[9]
    assert immediate_win_cells(("AB", ""), 1) == {"C"}
    assert immediate_win_cells(("AE", ""), 1) == {"I"}
    assert immediate_win_cells(("AB", "C"), 1) == set()  # opp blocks the line

    print("sanity asserts passed")
    print()
    print("monte_carlo from empty board, P1 to move, n=20000, seed=0:")
    print(" ", monte_carlo(("", ""), 1, 20_000, seed=0))
    print()
    print("monte_carlo from ('FH', 'J'), P1 to move, n=20000, seed=1:")
    print(" ", monte_carlo(("FH", "J"), 1, 20_000, seed=1))
    print()
    print("line_features for ('FH', 'J'), player=1:")
    for row in line_features(("FH", "J"), 1):
        print(" ", row)
