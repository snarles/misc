import pandas as pd
import numpy as np
from numpy.random import default_rng
rng = default_rng()

words = pd.read_csv("words500.txt")
wds = words.word.values
wds2i = {w:i for i,w in enumerate(wds)}
table = pd.read_csv("letters_penta.txt")
tablr = pd.read_csv("letters_reverses.txt")
auto_sort_lib = True

lat2heb = {'A': 'א',
 'B': 'ב',
 'G': 'ג',
 'D': 'ד',
 'H': 'ה',
 'V': 'ו',
 'Z': 'ז',
 'J': 'ח',
 'Y': 'ט',
 'I': 'י',
 'X': 'כ',
 'L': 'ל',
 'M': 'מ',
 'N': 'נ',
 'S': 'ס',
 'E': 'ע',
 'P': 'פ',
 'C': 'צ',
 'Q': 'ק',
 'R': 'ר',
 'W': 'ש',
 'T': 'ת'}
finals = {'X': 'ך',
 'M': 'ם',
 'N': 'ן',
 'P': 'ף',
 'C': 'ץ'}

def word2heb(w):
    if " " in w:
        ws = w.split(" ")
        return(word2heb(ws[0]) + " " + word2heb(ws[1]))
    w2 = ""
    for i in range(len(w)):
        l = w[i]
        if i==len(w)-1:
            if l in finals.keys() and len(w)>1:
                w2 += finals[l]
            else:
                w2 += lat2heb[l]
        else:
            w2 += lat2heb[l]
    return w2


loses_to = {table.loc[i, "letter"]: table.loc[i, "beats"] for i in range(26)}
reverses_to = {tablr.loc[i, "letter"]: tablr.loc[i, "reverses"] for i in range(26)}

letters = table.letter.values
# check for errors in letter table
for let1 in letters:
    for let2 in letters:
        if let1 != let2:
            w1 = let2 in loses_to[let1]
            w2 = let1 in loses_to[let2]
            if w1 == w2:
                print(let1, let2)
#for let in letters:
#    print(let + " " + str(len(loses_to[let])))

def wrap(w_short, w_long):
    w = w_short
    while len(w) < len(w_long):
        w+= w_short
    w = w[:len(w_long)]
    return w

def winner(w1, w2):
    w1 = w1.replace(" ", "")
    w2 = w2.replace(" ", "")
    if len(w1) > len(w2):
        return -winner(w2, w1)
    else:
        w1 = wrap(w1, w2)
        n_ties = 0
        for i in range(len(w2)):
            l1 = w1[i]
            l2 = w2[i]
            if l2 in reverses_to[l1]:
                n_ties += 1
        if n_ties % 2 == 0:
            return winner_wo_reverse(w1, w2)
        else:
            return -winner_wo_reverse(w1, w2)

def winner_wo_reverse(w1, w2):
    assert(len(w1) == len(w2))
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
                    won_yet=True
                    w1_first=1
            else:
                w1_adv -= 1
                if not won_yet:
                    won_yet=True
                    w1_first=-1
    if w1_adv == 0:
        return w1_first
    else:
        return np.sign(w1_adv)

def ai_fighter_based(rng, gamestate):
    cpu_active = gamestate["cpu_active"]
    cpu_inactive = gamestate["cpu_inactive"]
    pl_active = gamestate["pl_active"]
    pl_inactive = gamestate["pl_inactive"]
    move = {"move": "*"}
    if len(cpu_active)==0 or rng.random() < [0.0, 0.1, 0.2, 0.5, 0.8, 1.0][len(cpu_inactive)]:
        move = {"move": "*"}
        return move
    can_kill_flag = False
    n_tries = 0
    while not can_kill_flag and n_tries < 3:
        n_tries += 1
        chosen = rng.choice(cpu_active)
        cpu_target_inactive = False
        atargs = [w for w in pl_active if winner(chosen, w)>=0]
        itargs = [w for w in pl_inactive if winner(chosen, w)>=0]
        if len(atargs) + len(itargs) > 0:
            can_kill_flag = True
    chance = [0.0, 0.3, 0.6, 0.8, 1.0][len(itargs)]
    roll = rng.random()
    if roll < chance or len(atargs)==0:
        cpu_target_inactive = True
    if cpu_target_inactive:
        defeats = itargs
    else:
        if len(atargs) > 0:
            defeats = [rng.choice(atargs)]
        else:
            defeats = []
    if len(defeats)==0:
        if len(cpu_inactive) > 0:
            move = {"move": "*"}
            return move
        else:
            print("CPU " + word2heb(chosen) + " cannot defeat any of your words.")
            move = {"move": "*"}
            return move
    else:
        move["move"] = "fight"
        move["chosen"] = chosen
        move["defeats"] = defeats
        return move

def ai_target_based(rng, gamestate):
    cpu_active = gamestate["cpu_active"]
    cpu_inactive = gamestate["cpu_inactive"]
    pl_active = gamestate["pl_active"]
    pl_inactive = gamestate["pl_inactive"]
    move = {"move": "*"}
    if len(cpu_active)==0 or rng.random() < [0.0, 0.1, 0.2, 0.5, 0.8, 1.0][len(cpu_inactive)]:
        move = {"move": "*"}
        return move
    # look for a fighter that can win vs pl
    if len(pl_inactive) > 1 or len(pl_active)==0:
        search_order = rng.choice(cpu_active, len(cpu_active), replace=False)
        n_kills = []
        ind = 0
        not_found_flag = True
        # iterate through fighters
        while not_found_flag and ind < len(search_order):
            fighter = search_order[ind]
            n_kills.append(len([targ for targ in pl_inactive if winner(fighter, targ) >= 0]))
            if n_kills[-1] == len(pl_inactive):
                not_found_flag = False
            else:
                ind += 1
        #print(list(zip(search_order[:len(n_kills)], n_kills)))
        # if can kill at least 2, choose the best
        if np.max(n_kills) > 1 or len(pl_active)==0:
            chosen = search_order[np.argmax(n_kills)]
            move["move"] = "fight"
            move["chosen"] = chosen
            move["defeats"] = [w for w in pl_inactive if winner(chosen, w)>=0]
            return move
    # otherwise, choose a random opponent fighter and try to defeat
    targ = rng.choice(pl_active)
    can_beat_a = [w for w in cpu_active if winner(w, targ)>=0]
    can_beat_i = [w for w in cpu_inactive if winner(w, targ)>=0]
    if len(can_beat_a)==0 and len(can_beat_i)==0:
        # no way to win
        move["move"] = "resign"
        return move
    elif len(can_beat_a)==0:
        move["move"] = "*"
        return move
    else:
        move["move"] = "fight"
        move["chosen"] = rng.choice(can_beat_a)
        move["defeats"] = [targ]
        return move

diagnostic = []#["threat"]

def threat(chosen, team):
    return len([w for w in team if winner(w, chosen)>=0])

def cpu_eval_threat(current_a, current_i, next_a, next_i, eps=0.0001):
    '''Compute an evaluation function for outcome of a move based on threat formula'''
    te = -1.5 # threat exponent
    td = 0.8 # time discount
    dm = 1.3 # defensive multiplier : how much to multiply the score of the opponent
    cteam = current_a + current_i
    nteam = next_a + next_i
    cas = [(threat(w, nteam)+eps)**te for w in current_a] # current_a score
    cis = [(threat(w, nteam)+eps)**te for w in current_i] # current_i score
    nas = [(threat(w, cteam)+eps)**te for w in next_a] # next_a score
    nis = [(threat(w, cteam)+eps)**te for w in next_i] # next_i score
    # opponent threat minus own threat
    score = dm * (np.sum(nas) + td * np.sum(nis)) - (np.sum(cas) + td * np.sum(cis))
    if "threat" in diagnostic:
        print("score %0.3f" % score + " cas %0.3f" % np.sum(cas) + " cis %0.3f" % np.sum(cis) + " nas %0.3f" % np.sum(nas) + " nis %0.3f"% np.sum(nis))
    return score

def cpu_move_update(gamestate, move):
    '''Updates the game state given a cpu move'''
    nextstate = {}
    if move["move"]=="*":
        nextstate["cpu_active"] = gamestate["cpu_active"] + gamestate["cpu_inactive"]
        nextstate["cpu_inactive"] = []
        nextstate["pl_active"] = gamestate["pl_active"]
        nextstate["pl_inactive"] = gamestate["pl_inactive"]
    else:
        chosen = move["chosen"]
        defeats = move["defeats"]
        nextstate["cpu_active"] = lsub(gamestate["cpu_active"], [chosen])
        nextstate["cpu_inactive"] = gamestate["cpu_inactive"] + [chosen]
        nextstate["pl_active"] = lsub(gamestate["pl_active"], defeats)
        nextstate["pl_inactive"] = lsub(gamestate["pl_inactive"], defeats)
    return nextstate

def cpu_potential_moves(gamestate):
    moves = [{"move":"*"}]
    for w in gamestate["cpu_active"]:
        for w2 in gamestate["pl_active"]:
            if winner(w,w2) >= 0:
                moves.append({"move":"fight", "chosen":w, "defeats":[w2]})
        itargs = [w2 for w2 in gamestate["pl_inactive"] if winner(w, w2)>=0]
        if len(itargs) > 0:
            moves.append({"move":"fight", "chosen":w, "defeats":itargs})
    return moves

def lsub(list1, list2):
    '''List subtraction'''
    return [v for v in list1 if v not in list2]

def ai_threat_based(rng, gamestate, allow_resign=True, eps=0.0001):
    '''Chooses the move that results in the least threatening board state for CPU'''
    cpu_active = gamestate["cpu_active"]
    cpu_inactive = gamestate["cpu_inactive"]
    pl_active = gamestate["pl_active"]
    pl_inactive = gamestate["pl_inactive"]
    move = {"move": "*"}
    # generate all possible moves
    moves = cpu_potential_moves(gamestate)
    threatvals = []
    for mv in moves:
        if "threat" in diagnostic:
            print(mv)
        ns = cpu_move_update(gamestate, mv)
        threatvals.append(cpu_eval_threat(ns["cpu_active"], ns["cpu_inactive"], ns["pl_active"], ns["pl_inactive"], eps))
    if np.min(threatvals) > 1000.0 and allow_resign: # any move results in massive threat -- give up
        if "threat" in diagnostic:
            print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
        return {"move":"resign"}
    # do not allow recovery if CPU guaranteed win
    if len(threatvals) > 1 and np.min(threatvals[1:]) < -10000.0: # there is a non-recovery move that is massively safe
        threatvals[0] = 0.0
    if "threat" in diagnostic:
        print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
    return moves[np.argmin(threatvals)]

def ai_threat_based_subsample(rng, gamestate, n_samples=5):
    gamestate2 = {}
    gamestate2["cpu_active"] = list(rng.choice(gamestate["cpu_active"], min(n_samples, len(gamestate["cpu_active"])), replace=False))
    gamestate2["pl_active"] = list(rng.choice(gamestate["pl_active"], min(n_samples, len(gamestate["pl_active"])), replace=False))
    gamestate2["cpu_inactive"] = gamestate["cpu_inactive"]
    gamestate2["pl_inactive"] = gamestate["pl_inactive"]
    return ai_threat_based(rng, gamestate2, allow_resign=False, eps=0.5)

def sort_word_list(wl):
    return list(wds[np.sort([wds2i[w] for w in wl])])

size_team = 11
current_level = 5
n_multi_armies = [1]*10 + [2]*20 + [3]*20

print("--------------------------")
print("    Elimination Game      ")
print("--------------------------")

print("1. New game")
print("2. Continue")

x = input()

if x=="1":
    n_lib = 30 # number of words in pool to draw from
    flag = True
    while flag:
        print("===New team:===")
        w_pool = wds[:n_lib]
        player_lib = list(w_pool[np.sort(rng.choice(n_lib, size_team, replace=False))])
        player_team = player_lib.copy()
        for w in player_lib:
            print(word2heb(w))
        print("Accept this team? (y)")
        y = input()
        if y=="y":
            flag = False
elif x=="2":
    print("===Paste library:===")
    y = input()
    player_lib = y.split(",")

    print("===Paste team:===")
    y = input()
    player_team = y.split(",")

    check_flag = True
    while check_flag:
        print("===Paste level:===")
        z = int(input())

        print("===Paste passcode:===")
        w = int(input())

        r2 = default_rng(z*9876)
        w2 = r2.choice(1000000)
        if w2==w:
            check_flag = False
            current_level = z

# generate opponents
r0 = default_rng(0)
multi_armies = {}
for j in range(3):
    armies = []
    for i in range(21):
        sub_wds = wds[:min(len(wds),(i*10+20))]
        armies.append(sort_word_list(r0.choice(sub_wds, size_team, False)))
    multi_armies[j] = armies

game_flag=True

while game_flag:
    if auto_sort_lib:
        player_lib = sort_word_list(player_lib)
    check_flag = True
    while check_flag:
        print("==Choose an opponent (1-%i), 0 to quit==" % current_level)
        z = int(input())
        if z== 0:
            print("==Player army==")
            print(",".join(player_lib))
            print("==Current team==")
            print(",".join(player_team))
            print("==Current level==")
            print(current_level)
            print("==Passcode==")
            r2 = default_rng(current_level*9876)
            w2 = r2.choice(1000000)
            print(w2)
            game_flag=False
        if z > current_level:
            print("Too high")
        else:
            check_flag = False
    if z > 0:
        print("==Opponent %i==" % z)
        m = n_multi_armies[z]
        for i in range(m):
            print("==CPU team %i-%i==" % (z, i+1))
            print(", ".join([word2heb(w) for w in multi_armies[i][z]]))
        input()
        print("==Player army==")
        print(",".join([word2heb(w) for w in player_lib]))
        print("==Current team==")
        print(",".join([w for w in player_team]))
        print("==Choose your team (words separated by commas w/o spaces, press enter to keep current)==")
        ar = input()
        if len(ar) > 0:
            army = ar.split(",")
            army = list(np.unique(np.array(army)))
            valid_flag = True
            for w in army:
                if w not in player_lib:
                    print("%s not in library." % w)
                    valid_flag = False
            if len(army) != size_team:
                print("Team needs %i unique words." % size_team)
                valid_flag = False
            if valid_flag:
                player_team = army
        current_opp = -1
        war_flag=True
        while war_flag:
            current_opp += 1
            # setup one opponent
            cpu_active = list(multi_armies[current_opp][z])
            cpu_inactive = []
            pl_active = player_team
            pl_inactive = []
            battle_flag = True
            while battle_flag:
                resign_flag = False
                print("\n\n\n\n\n")
                print("==CPU active==")
                print(", ".join([word2heb(w) for w in cpu_active]))
                print("==CPU inactive==")
                print(", ".join([word2heb(w) for w in cpu_inactive]))
                print("")
                print("---")
                print("")
                print("==Player active==")
                print(", ".join([word2heb(w) for w in pl_active]))
                print("==Player inactive==")
                print(", ".join([word2heb(w) for w in pl_inactive]))
                print("==Choose a fighter (* for recovery, @ to resign)==")
                choose_flag = True
                while choose_flag:
                    xx = input()
                    if xx in pl_active:
                        choose_flag = False
                    if xx == "*" or xx == "@":
                        choose_flag = False
                if xx == "*":
                    print("*** RECOVERY ***")
                    pl_active = pl_active + pl_inactive
                    pl_inactive = []
                    input()
                elif xx == "@":
                    print("@@@ You resign! @@@")
                    resign_flag = True
                    input()
                else:
                    chosen = xx
                    pl_active = [w for w in pl_active if w != chosen]
                    pl_inactive.append(chosen)
                    print("==Fight==")
                    print("Enter list of opponents to fight, separated by commas without spaces.")
                    print("You can fight one active opponent or any number of inactive opponents.")
                    target_flag = True
                    while target_flag:
                        xx = input()
                        if "," in xx:
                            targs = xx.split(",")
                            valid_flag = True
                            for t in targs:
                                if t not in cpu_inactive:
                                    valid_flag = False
                            if valid_flag:
                                target_flag = False
                        else:
                            if xx in cpu_active or xx in cpu_inactive:
                                target_flag = False
                                targs = [xx]
                    for target in targs:
                        print("Your " + word2heb(chosen) + " vs. CPU " + word2heb(target))
                        w_or_not = winner(chosen, target)
                        if w_or_not >= 0:
                            print(word2heb(target) + " eliminated.")
                            cpu_active = [w for w in cpu_active if w != target]
                            cpu_inactive = [w for w in cpu_inactive if w != target]
                        elif w_or_not == -1:
                            print("...but " + word2heb(chosen) + " lost!")
                    input()
                if len(cpu_active) + len(cpu_inactive)==0:
                    battle_flag=False
                    print("@@@ VICTORY!! @@@")
                    w_pool = wds[:min(len(wds),(z*10+20))]
                    w_pool = list(set(w_pool).difference(set(player_lib)))
                    if len(w_pool) > 0:
                        w_new = rng.choice(w_pool)
                        print("You gain a word: " + word2heb(w_new))
                        player_lib.append(w_new)
                    if current_opp == m-1:
                        print("@@@ DEFEATED ALL TEAMS! @@@")
                        war_flag = False
                        if z > current_level -5:
                            print("Level up!")
                            current_level = z + 5
                    input()
                elif resign_flag == True:
                    battle_flag = False
                    pl_active = []
                    pl_inactive = []
                else:
                    # CPU's turn
                    gamestate = {
                        "cpu_active": cpu_active,
                        "cpu_inactive": cpu_inactive,
                        "pl_active": pl_active,
                        "pl_inactive": pl_inactive
                    }
                    if len(cpu_active + cpu_inactive + pl_active + pl_inactive) > 10:
                        cpu_ai = lambda rng, gamestate : ai_threat_based_subsample(rng, gamestate, 5)
                    else:
                        cpu_ai = ai_threat_based
                    if z <= 1:
                        cpu_ai = ai_fighter_based
                    elif z <= 3:
                        cpu_ai = ai_target_based
                    cpu_move = cpu_ai(rng, gamestate)
                    if cpu_move["move"] == "*":
                        print("===** CPU recovery **===")
                        cpu_active = cpu_active + cpu_inactive
                        cpu_inactive = []
                        input()
                    elif cpu_move["move"] == "resign":
                        battle_flag=False
                        print("CPU resigns.")
                        print("@@@ VICTORY!! @@@")
                        w_pool = wds[:min(len(wds),(z*10+20))]
                        w_pool = list(set(w_pool).difference(set(player_lib)))
                        if len(w_pool) > 0:
                            w_new = rng.choice(w_pool)
                            print("You gain a word: " + word2heb(w_new))
                            player_lib.append(w_new)
                        if current_opp == m-1:
                            print("@@@ DEFEATED ALL TEAMS! @@@")
                            war_flag = False
                            if z > current_level -5:
                                print("Level up!")
                                current_level = z + 5
                        input()
                    else:
                        chosen = cpu_move["chosen"]
                        defeats = cpu_move["defeats"]
                        cpu_active = [w for w in cpu_active if w != chosen]
                        cpu_inactive.append(chosen)
                        for target in defeats:
                            print("CPU " + word2heb(chosen) + " vs. your " + word2heb(target))
                            print(word2heb(target) + " eliminated.")
                            pl_active = [w for w in pl_active if w != target]
                            pl_inactive = [w for w in pl_inactive if w != target]
                            input()
                if len(pl_active) + len(pl_inactive)==0:
                    print("@@@ You lost! @@@")
                    n_kills = size_team - len(cpu_active + cpu_inactive)
                    print("But you managed to defeat %i out of %i of the opponent's words." % (n_kills, size_team))
                    input()
                    if rng.random() < (n_kills/size_team):
                        w_pool = wds[:min(len(wds),(z*10+20))]
                        w_pool = list(set(w_pool).difference(set(player_lib)))
                        if len(w_pool) > 0:
                            w_new = rng.choice(w_pool)
                            print("You gain a word: " + word2heb(w_new))
                            player_lib.append(w_new)
                        input()
                    battle_flag=False
                    war_flag=False
