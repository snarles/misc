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
all_doubs = []
for ii in range(3):
    doubs = [cd[double_pos[ii][0]] + cd[double_pos[ii][1]] for cd in ucodons]
    all_doubs.append(np.array(doubs))
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

def disp_counters(codons, gs, pl, limit=4, verbose=True):
    pos = min(limit, len(gs[pl]))
    if pos == 0:
        cands = codons
    else:
        flag = True
        while flag:
            cands = filter_codons(codons, gs[pl][-pos:])
            if len(cands) > 0:
                flag = False
            else:
                pos -= 1
    if verbose:
        print(pos)
        print(",".join(cands))
    return cands

def disp_nbeat(gs, pl, verbose=True):
    oppo = {"pl1":"pl2", "pl2":"pl1"}
    nb = nbeat(gs[pl][-1], gs[oppo[pl]])
    if verbose:
        print(pl.upper() + " scores " + str(nb)+ " with "+gs[pl][-1])
    return nb

def forecast_counters(gs, pl1='', pl2='', limit=4, verbose=True):
    p1c = gs['pl1']
    p2c = gs['pl2']
    if pl1 != '':
        p1c = np.concatenate((p1c, [pl1]))
    if pl2 != '':
        p2c = np.concatenate((p2c, [pl2]))
    if len(p1c) == len(p2c):
        curr = 'pl2'
        nb = len(p2c)
    else:
        curr = 'pl1'
        nb = len(p1c)
    gs2 = {'pl1':p1c, 'pl2':p2c}
    counters = disp_counters(remain_codons(gs2), gs2, curr, limit, verbose)
    return counters

## Game 4
# 6x pre-draft
# restrict-6 for first 50 turns, then restrict-5

#     P1         P2 (goes first)
#     - pre-draft -
# p1- DRA  (0)   CAM  (1)
# p2- COO  (1)   BLP  (2)
# p3- RUR  (2)   STK  (3)
# p4- TNR  (3)   HES  (4)
# p5- THW  (4)   GOW  (4)
# p6- DOS  (4)   WIW  (4)
#  1- PL4  (4)   CAA  (3)
#  2- SHU  (4)   NHB  (4)
#  3- PAB  (4)   EG1  (4)
#  4- DR4  (4)        (
#  5-      ( )        (
#  6-      ( )        (
#     -remove pre-draft-
# [Takebacks]
# @p4b, p1b. CAM -> b/c p2a. COO too strong

def remain_codons(gs):
    used = np.concatenate((gs['pl1'], gs['pl2']))
    if np.sum(used == "COC") >= 3:
        used = np.unique(used)
    else:
        used = np.unique(list(set(used).difference(set(["COC"]))))
    rcodons = np.sort(list(set(ucodons).difference(set(used))))
    return rcodons

# remain_codons({'pl1': ['DRA', 'COC'], 'pl2': ['COC']})
# remain_codons({'pl1': ['DRA', 'COC'], 'pl2': ['COC', 'COC']})

gs = {'pl1': ['DRA'], 'pl2': []}
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 1
# ANM,BEF,BLP,BRA,BUB,CAM,COC,D2U,D3P,DOA,DR4,DR5,FAL,GOM,GRO,HES,HL1,ILL,ING,INW,JAC,KL4,LKD,LOK,LWB,MUL,NHB,PAB,PL4,ROR,RUR,SCB,SHA,SHB,SPG,STK,TES,TEU,THO,WIW,WOP

gs['pl2'].append('CAM')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 1
# ANM,BLP,BRA,BRF,COO,D2U,FAL,FUK,GAD,GOM,GRO,HAW,HL1,HL2,HL3,HUL,ING,INW,JAC,KL3,KL4,LDT,MOC,MUL,NHB,PL2,PL3,PL4,PLA,POV,PYP,RAT,ROR,RUR,SCB,SHB,SPM,STK,STL,THA,THW,TNR,TRH,TSH,WAF,WAS,WIW

gs['pl1'].append('COO')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 2
# BEF,BLP,BUB,DOA,DR4,HES,ILL,LKD,SPG,STK,TEU

gs['pl2'].append('BLP')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 2
# FUK,HL1,HL2,HL3,HUL,MOC,MUL,NHB,POV,RAT,RUR,SHB,STL,THW,TNR,TRH,TSH,WAS,WIW

# offensive scores
scores = np.array([len(forecast_counters(gs, pl1=cd, verbose=False)) for cd in counters])
print((np.min(scores), counters[scores == np.min(scores)]))
# (2, array(['RUR'], dtype='<U3'))

gs['pl1'].append('RUR')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 3
# HES,STK

fc = forecast_counters(gs, pl2="HES")
# 3
# MOC,MUL,RAT,THW,TRH,TSH,WAS,WIW
fc = forecast_counters(gs, pl2="STK")
# 3
# HL1,SHB,STL,THW,TNR,TRH,TSH

gs['pl2'].append('STK')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 3
# HL1,SHB,STL,THW,TNR,TRH,TSH

scores = np.array([len(forecast_counters(gs, pl1=cd, verbose=False)) for cd in counters])
print((np.min(scores), counters[scores == np.min(scores)]))
# (1, array(['HL1', 'SHB', 'STL', 'TNR'], dtype='<U3'))

gs['pl1'].append('TNR')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 4
# HES

gs['pl2'].append('HES')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 4
# THW,TRH,TSH

scores = np.array([len(forecast_counters(gs, pl1=cd, verbose=False)) for cd in counters])
print((np.min(scores), counters[scores == np.min(scores)]))
# (1, array(['THW', 'TRH'], dtype='<U3'))
gs['pl1'].append('THW')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 4
# GOW

gs['pl2'].append('GOW')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 4
# DOS,ILL,SHU,SPG,THO

scores = np.array([len(forecast_counters(gs, pl1=cd, verbose=False)) for cd in counters])
print((np.min(scores), counters[scores == np.min(scores)]))
# (1, array(['DOS', 'SHU'], dtype='<U3'))

gs['pl1'].append('DOS')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 4
# WIW

gs['pl2'].append('WIW')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 4
# BRF,GAD,GRO,ILL,PL4,SHU,SPG,THO
fc = forecast_counters(gs, pl1="PL4")
fc = forecast_counters(gs, pl1="GRO")
fc = forecast_counters(gs, pl1="SHU")
fc = forecast_counters(gs, pl1="SPG")
fc = forecast_counters(gs, pl1="THO")
# 3
# CAA,DR4,NHB,SPM
# 3
# ANM,CAA,DID,SPM,TEU,UWY,WES
# 4
# DR3,PAB,THA
# 3
# ANM,CAA,DR4,SHU,UWY
# 4
# PAB
gs['pl1'].append('PL4')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 3
# CAA,DR4,NHB,SPM
fc = forecast_counters(gs, pl2="CAA")
fc = forecast_counters(gs, pl2="DR4")
fc = forecast_counters(gs, pl2="SPM")
# 4
# ILL,MOC,RAT,SHU,THO
# 4
# BUB,GRO,ILL,PL2,TES,TEU
# 4
# BRF,BUB,GAD,PL2,SHU,SPG
gs['pl2'].append('CAA')
counters = disp_counters(remain_codons(gs), gs, "pl2")
# 4
# ILL,MOC,RAT,SHU,THO
fc = forecast_counters(gs, pl1="SHU")
fc = forecast_counters(gs, pl1="THO")
# 4
# DR4,NHB
# 4
# DR4,NHB,SPM
gs['pl1'].append('SHU')
fc = forecast_counters(gs, pl2="DR4")
fc = forecast_counters(gs, pl2="NHB")
# 4
# ILL,LDT,LKD,PAB,SHA,SHB
# 4
# ILL,ING,LDT,PAB,SHA,SHB
gs['pl2'].append('NHB')
fc = forecast_counters(gs, pl1="PAB")
# 4
# DBM,DR2,EG1
fc = forecast_counters(gs, pl2="DBM")
fc = forecast_counters(gs, pl2="DR2")
fc = forecast_counters(gs, pl2="EG1")
# 4
# DR4
# 4
# DR4
# 4
# DR4
gs['pl2'].append('EG1')
gs['pl1'].append('DR4')
counters = disp_counters(remain_codons(gs), gs, "pl1")
# 4
# DR2,GOM,LKD
print(gs)
