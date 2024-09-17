import pandas as pd
import numpy as np
from numpy.random import default_rng
rng = default_rng()

words = pd.read_csv("words500.txt")
wds = words.word.values
table = pd.read_csv("letters.txt")

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
            if l in finals.keys():
                w2 += finals[l]
            else:
                w2 += lat2heb[l]
        else:
            w2 += lat2heb[l]
    return w2


loses_to = {table.loc[i, "letter"]: table.loc[i, "beats"] for i in range(22)}

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

n_army = 11
current_level = 3
n_multi_armies = [1]*10 + [2]*20 + [3]*20

print("--------------------------")
print("    Elimination Game      ")
print("--------------------------")

print("1. New game")
print("2. Continue")

x = input()

if x=="1":
    n_lib = 50 # number of words in pool to draw from
    flag = True
    while flag:
        print("===New team:===")
        w_pool = wds[:n_lib]
        player_lib = list(rng.choice(w_pool, n_army, replace=False))
        player_team = player_lib
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
    for i in range(13):
        armies.append(r0.choice(wds[:(i*10+20)], 11, False))
    multi_armies[j] = armies

game_flag=True

while game_flag:
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
        print(",".join([word2heb(w) for w in player_team]))
        print("==Choose your team (words separated by commas w/o spaces, press enter to keep current)==")
        ar = input()
        if len(ar) > 0:
            army = ar.split(",")
            army = list(np.unique(np.array(army)))
            valid_flag = True
            for w in army:
                if w not in player_lib:
                    valid_flag = False
            if len(army) != n_army:
                print("Team needs %i unique words." % n_army)
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
                print("==Choose a fighter (* for recovery)==")
                choose_flag = True
                while choose_flag:
                    xx = input()
                    if xx in pl_active:
                        choose_flag = False
                    if xx == "*":
                        choose_flag = False
                if xx == "*":
                    print("*** RECOVERY ***")
                    pl_active = pl_active + pl_inactive
                    pl_inactive = []
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
                            print("...but " + word2heb(chosen) + " tied!")
                    input()
                if len(cpu_active) + len(cpu_inactive)==0:
                    battle_flag=False
                    print("@@@ VICTORY!! @@@")
                    w_pool = wds[:(z+2)*10]
                    w_pool = list(set(w_pool).difference(set(player_lib)))
                    if len(w_pool) > 0:
                        w_new = rng.choice(w_pool)
                        print("You gain a word: " + word2heb(w_new))
                        player_lib.append(w_new)
                    if current_opp == m-1:
                        print("@@@ DEFEATED ALL TEAMS! @@@")
                        war_flag = False
                        current_level += 1
                    input()
                else:
                    # CPU's turn
                    if rng.random() < [0.0, 0.1, 0.5, 0.7, 0.9, 1.0][len(cpu_inactive)]:
                        print("===** CPU recovery **===")
                        cpu_active = cpu_active + cpu_inactive
                        cpu_inactive = []
                        input()
                    else:
                        chosen = rng.choice(cpu_active)
                        cpu_active = [w for w in cpu_active if w != chosen]
                        cpu_inactive.append(chosen)
                        cpu_target_inactive = False
                        atargs = [w for w in pl_active if winner(chosen, w)>=0]
                        itargs = [w for w in pl_inactive if winner(chosen, w)>=0]
                        chance = [0.0, 0.3, 0.5, 0.8, 1.0][len(itargs)]
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
                                print("CPU " + word2heb(chosen) + " cannot defeat any of your words.")
                        for target in defeats:
                            print("CPU " + word2heb(chosen) + " vs. your " + word2heb(target))
                            print(word2heb(target) + " eliminated.")
                            pl_active = [w for w in pl_active if w != target]
                            pl_inactive = [w for w in pl_inactive if w != target]
                            input()
                if len(pl_active) + len(pl_inactive)==0:
                    battle_flag=False
                    war_flag=False
