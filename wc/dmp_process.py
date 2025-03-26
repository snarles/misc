import numpy as np
import os as os
import pandas as pd

table = pd.read_csv("letters_wc_beats.txt")
tablr = pd.read_csv("letters_wc_reverses.txt")

loses_to = {table.loc[i, "letter"]: table.loc[i, "beats"] for i in range(len(table))}
reverses_to = {tablr.loc[i, "letter"]: tablr.loc[i, "reverses"] for i in range(len(table))}
letters = table.letter.values

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


fs = [f for f in os.listdir("../dmp/") if f[-4:]==".txt"]

cards = []
for f in fs:
    tx = np.loadtxt("../dmp/" + f, dtype=str, delimiter="@")
    for ss in tx:
        if "(" in ss or "●" in ss:
            0
        else:
            sss = []
            ss = ss.replace(" ☆","")
            if "/" in ss:
                sss = ss.split("/")
            else:
                sss = [ss]
            for cn in sss:
                while cn[0] == " ":
                    cn = cn[1:]
                while cn[-1] == " ":
                    cn = cn[:-1]
                cards.append(cn)

cards = np.unique(cards)
cards = np.array(list(set(cards).difference(set(["Light","Water","Darkness","Fire","Nature","Multicolored","Zero"]))))

def codon(cn):
    cn = cn.replace('.','')
    cn = cn.replace('&','')
    cn = cn.replace('"','')
    cn = cn.replace("~","")
    cn = cn.replace("-"," ")
    cn = cn.upper()
    cn = cn.replace("Ü","U")
    pts = cn.split(" ")
    pts = [pt for pt in pts if pt not in ["THE","OF","AND",""]]
    if len(pts)==1:
      return pts[0][:3]
    if len(pts)==2:
      if len(pts[0]) > 1:
        return pts[0][:2] + pts[1][0]
      else:
        return pts[0][0] + pts[1][:2]
    if len(pts) > 2:
      return pts[0][0] + pts[1][0] + pts[2][0]
    
codons = []
for cn in cards:
    codons.append(codon(cn))
codons = np.array(codons)

codon_tab = np.array(list(zip(codons, cards)))
ucodons = np.unique(codons)
codon_dict = {}
for cd in ucodons:
    matches = cards[codons==cd]
    codon_dict[cd] = matches

nmatches = np.array([len(codon_dict[cd]) for cd in ucodons])




# display most promising 2-letter/position combinations
import pandas as pd
double_pos = [[1,2],[0,2],[0,1]]
for ii in range(3):
    doubs = [cd[double_pos[ii][0]] + cd[double_pos[ii][1]] for cd in ucodons]
    vc = pd.Series(doubs).value_counts()
    temp = ["?","?","?"]
    temp[double_pos[ii][0]] = "1"
    temp[double_pos[ii][1]] = "2"
    print(temp[0]+temp[1]+temp[2])
    print(vc[:np.sum(vc > 15)])


#for ii in range(50):
#    cd1 = choice(ucodons)
#    cd2 = choice(ucodons)
#    w = winner(cd1,cd2)
#    print(cd1 + " vs " + cd2 + ": " + str(w))


gs = {"pl1":[],"pl2":[]}


def filter_codons(cods, cset):
    cands = []
    for cod in cods:
        ww = np.array([winner(cod, cd) for cd in cset])
        if np.min(ww) == 1:
            cands.append(cod)
    return np.array(cands)

#filter_codons(ucodons, cset)

from numpy.random import choice

def pick_card(gs, max_disp=50):
    if len(gs["pl1"]) == len(gs["pl2"]):
        cset = gs["pl2"]
        player = "pl1"
    else:
        cset = gs["pl1"]
        player = "pl2"
    if len(cset) == 0:
        cands = ucodons
    else:
        pos = min(8,len(cset))
        flag = True
        while flag:
            cands = filter_codons(ucodons, cset[-pos:])
            if len(cands) == 0:
                pos -= 1
            else:
                flag=False
    if len(cands) > max_disp:
        cands = choice(cands, max_disp, replace=False)
    for cd in cands:
        print(cd + " " + "; ".join(codon_dict[cd]))
    cd = input()
    gs[player].append(cd)
    return gs

#pick_card(gs)
