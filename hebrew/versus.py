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
            if l in finals.keys() and len(w)>1:
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

print("Type ? and enter if you want an explanation.")

for i in range(10):
    print(" ")
    i1 = rng.choice(len(wds))
    i2 = rng.choice(len(wds))
    w1 = words.loc[i1, "word"]
    w2 = words.loc[i2, "word"]
    print(word2heb(w1))
    print(word2heb(w2))
    a=input()
    if a == "?":
        w1 = w1.replace(" ","")
        w2 = w2.replace(" ","")
        max_len = max(len(w1), len(w2))
        for j in range(max_len):
            l1 = w1[j % len(w1)]
            l2 = w2[j % len(w2)]
            if l1 in loses_to[l2]:
                print(lat2heb[l1] + " loses to " + lat2heb[l2])
            elif l2 in loses_to[l1]:
                print(lat2heb[l1] + " wins vs. " + lat2heb[l2])
            else:
                print(lat2heb[l1] + " ties vs. " + lat2heb[l2])
    who_win = winner(w1, w2)
    if who_win == 1:
        print(word2heb(w1) + " wins")
    elif who_win == -1:
        print(word2heb(w2) + " wins")
    else:
        print("tied")
    a = input()
    if a == "?":
        print(" ")
        for i in [i1, i2]:
            print(word2heb(words.loc[i, "word"]) + " " + words.loc[i, "pronunciation"] + " " + words.loc[i, "gloss"])
