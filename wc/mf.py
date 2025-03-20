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



def filter_codons(cods, cset):
    cands = []
    for cod in cods:
        ww = np.array([winner(cod, cd) for cd in cset])
        if np.min(ww) == 1:
            cands.append(cod)
    return np.array(cands)

def nbeat(cd, cset):
    pos = -1
    while (-pos <= len(cset)) and winner(cd, cset[pos]) == 1:
        pos -= 1
    return -pos - 1

#filter_codons(ucodons, cset)

from numpy.random import choice

#pick_card(gs)

kee="ANM,BLP,BLW,BUB,CAA,CAM,DOS,DRA,FAL,GAM,GRO,HAW,HUL,ING,IRM,LOK,ROR,SHU,SPG,SPM,STL,THA,THO,WAS".split(",")
goa="AVE,BEF,BRA,BRF,CIW,COC,COC,COO,DBM,FAD,FUK,GAD,GOA,GUG,HOH,ILL,INW,MAM,MEG,MUL,POV,PYP,SCB,SGG,SHA,SHB,TES,WAF,WES,WIW,WOP".split(",")
act="ARE,COC,D2U,D3P,DID,EG1,EXK,JAC,LDT,LES,LWB,RAT,RUR,STK,TEU,TNR,TRH,TRK,TSH,UWY,ZAC".split(",")
ner="DOA,DR2,DR3,DR4,DR5,GOM,GOW,HES,HL1,HL2,HL3,KL3,KL4,LKD,MOC,NHB,PAB,PL2,PL3,PL4,PLA,SPS,THW".split(",")
gs = {"pl1":[],"pl2":[]}

codons = np.concatenate([kee,goa,act,ner])
ucodons = np.unique(codons)

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
    print(vc[:np.sum(vc > 1)])

# ?12
# ES    4
# L3    3
# AM    3
# RA    2
# IW    2
# L2    2
# OC    2
# L4    2
# AD    2
# UL    2
# HB    2
# HA    2
# OA    2
# AC    2
# dtype: int64
# 1?2
# TH    2
# GM    2
# WS    2
# RR    2
# BF    2
# SG    2
# DA    2
# SB    2
# AE    2
# dtype: int64
# 12?
# DR    5
# PL    4
# SH    3
# SP    3
# HL    3
# GO    3
# TH    3
# CA    2
# BL    2
# WA    2
# ST    2
# BR    2
# TE    2
# FA    2
# KL    2
# CO    2
# DO    2
# GA    2
# IN    2
# TR    2
# dtype: int64


## Game 1

#     P2         P1
#     - pre-draft -
# p1- LES  (0)   KL3  (1)
# p2- WES  (1)   DOA  (2)
# p3- HES  (1)   COCu (3)
# p4- SPS  (3)   CAA  (4)
# p5- HL2  (4)   BRA  (5)
# p6- SHB  (5)   D3P  (4)
#     --- draft ---




def disp_counters(gs, pl, nb=0):
    pos = -len(gs[pl])
    old_cands = []
    while pos <= -nb and pos < 0:
        new_cands = filter_codons(ucodons, gs[pl][pos:])
        print_cands = np.sort(np.array(list(set(new_cands).difference(set(old_cands)))))
        if len(print_cands) > 0:
            print(-pos)
            print(",".join(print_cands))
        old_cands = np.concatenate([old_cands, new_cands])
        pos += 1

def disp_nbeat(gs, pl):
    oppo = {"pl1":"pl2", "pl2":"pl1"}
    nb = nbeat(gs[pl][-1], gs[oppo[pl]])
    print(pl.upper() + " scores " + str(nb)+ " with "+gs[pl][-1])
    return nb

gs = {'pl1': ['LES'], 'pl2': ['KL3']}
nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))

gs = {'pl1': ['LES', 'WES'], 'pl2': ['KL3']}
nb = disp_nbeat(gs, "pl1")
disp_counters(gs, "pl1", max(0, nb-1))

gs = {'pl1': ['LES', 'WES'], 'pl2': ['KL3', 'DOA']}
nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))

gs = {'pl1': ['LES', 'WES', 'HES'], 'pl2': ['KL3', 'DOA']}
nb = disp_nbeat(gs, "pl1")
disp_counters(gs, "pl1", max(0, nb-1))

gs = {'pl1': ['LES', 'WES', 'HES'], 'pl2': ['KL3', 'DOA', 'COC']}
nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))

gs['pl1'].append('SPS')
nb = disp_nbeat(gs, "pl1")
disp_counters(gs, "pl1", max(0, nb-1))

gs['pl2'].append('CAA')
nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))

gs['pl1'].append('HL2')
nb = disp_nbeat(gs, "pl1")
disp_counters(gs, "pl1", max(0, nb-1))

gs['pl2'].append('BRA')
nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))

gs['pl1'].append('SHB')
nb = disp_nbeat(gs, "pl1")
disp_counters(gs, "pl1", max(0, nb-1))

gs['pl2'].append('D3P')
gs = {
    'pl1': ['LES', 'WES', 'HES', 'SPS', 'HL2', 'SHB'],
    'pl2': ['KL3', 'DOA', 'COC', 'CAA', 'BRA', 'D3P']}

nb = disp_nbeat(gs, "pl2")
disp_counters(gs, "pl2", max(0, nb-1))
