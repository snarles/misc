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

def disp_counters(gs, pl, nb=0, verbose=True):
    pos = -len(gs[pl])
    old_cands = []
    while pos <= -nb and pos < 0:
        new_cands = filter_codons(ucodons, gs[pl][pos:])
        print_cands = np.sort(np.array(list(set(new_cands).difference(set(old_cands)))))
        if len(print_cands) > 0:
            if verbose:
                print(-pos)
                print(",".join(print_cands))
        old_cands = np.concatenate([old_cands, print_cands])
        pos += 1
    return old_cands

def disp_nbeat(gs, pl, verbose=True):
    oppo = {"pl1":"pl2", "pl2":"pl1"}
    nb = nbeat(gs[pl][-1], gs[oppo[pl]])
    if verbose:
        print(pl.upper() + " scores " + str(nb)+ " with "+gs[pl][-1])
    return nb

def forecast_counters(gs, pl1='', pl2='', nres=6):
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
    counters = disp_counters({'pl1':p1c, 'pl2':p2c}, curr, min(nres, nb), verbose=False)
    return counters

## Game 4
# 6x pre-draft
# restrict-6 for first 50 turns, then restrict-5

#     P1         P2 (goes first)
#     - pre-draft -
# p1- DRA  (0)   DR4  (1)
# p2- GRO  (1)   D2U  (2)
# p3-   (3)     (3)
# p4-   (4)   ---
# p5-
# p6-
#  1-
#  2-      ( )        ( )
# [Takebacks]
# @p4b, p1b. CAM -> b/c p2a. COO too strong

nres=6
gs = {'pl1': ['DRA'], 'pl2': []}
counters = disp_counters(gs, "pl1", 1)
# 1
# ANM,BEF,BLP,BRA,BUB,CAM,COC,D2U,D3P,DOA,DR4,DR5,FAL,GOM,GRO,HES,HL1,ILL,ING,INW,JAC,KL4,LKD,LOK,LWB,MUL,NHB,PAB,PL4,ROR,RUR,SCB,SHA,SHB,SPG,STK,TES,TEU,THO,WIW,WOP

## find good counters for DR?...

cands = ucodons[all_doubs[2]=="DR"]
score = np.array([np.sum([winner(cd, cand)==1 for cand in cands]) for cd in ucodons])
print(ucodons[score == np.max(score)])
print(ucodons[score == np.max(score)-1])
# ['CAM' 'FAL' 'LOK' 'MUL']
# ['BEF' 'BLP' 'BRA' 'BUB' 'COC' 'D2U' 'D3P' 'DOA' 'GRO' 'HES' 'HL1' 'JAC' 'NHB' 'ROR' 'RUR' 'SHA' 'SHB' 'STK' 'TES' 'TEU' 'WIW' 'WOP']

gs['pl2'].append('CAM')
counters = disp_counters(gs, "pl2", 1)
# 1
# ANM,BLP,BRA,BRF,COO,D2U,FAL,FUK,GAD,GOM,GRO,HAW,HL1,HL2,HL3,HUL,ING,INW,JAC,KL3,KL4,LDT,MOC,MUL,NHB,PL2,PL3,PL4,PLA,POV,PYP,RAT,ROR,RUR,SCB,SHB,SPM,STK,STL,THA,THW,TNR,TRH,TSH,WAF,WAS,WIW

gs['pl1'].append('COO')
counters = disp_counters(gs, "pl1", 2)
# 2
# BEF,BLP,BUB,DOA,DR4,HES,ILL,LKD,SPG,STK,TEU

gs['pl2'].append('BEF')
counters = disp_counters(gs, "pl2", 2)
# 2
# BRF,GAD,GOM,GRO,HAW,HL1,HL2,HUL,INW,KL4,LDT,MUL,NHB,PL2,PL4,PLA,POV,PYP,RAT,ROR,RUR,SHB,TNR,WAS

gs['pl1'].append('GRO')
counters = disp_counters(gs, "pl1", 3)
# 3
# BLP,BUB,ILL,SPG,TEU

gs['pl2'].append('BLP')
counters = disp_counters(gs, "pl2", 3)
# 3
# HL1,HL2,HUL,MUL,NHB,POV,RAT,RUR,SHB,TNR,WAS

print(filter_codons(counters, "BLP,BUB,ILL,SPG,TEU".split(",")))
# ['RUR']

gs['pl1'].append('RUR')
counters = disp_counters(gs, "pl1", 4)
#

## implement takebacks
gs = {'pl1': ['DRA', 'COO'], 'pl2': ['CAM']}
counters = disp_counters(gs, "pl1", 2)
# 2
# BEF,BLP,BUB,DOA,DR4,HES,ILL,LKD,SPG,STK,TEU

# forecasts
print(",".join(forecast_counters(gs, pl2='BEF')))
# BRF,GAD,GOM,GRO,HAW,HL1,HL2,HUL,INW,KL4,LDT,MUL,NHB,PL2,PL4,PLA,POV,PYP,RAT,ROR,RUR,SHB,TNR,WAS
print(",".join(forecast_counters(gs, pl2='BLP')))
# FUK,HL1,HL2,HL3,HUL,MOC,MUL,NHB,POV,RAT,RUR,SHB,STL,THW,TNR,TRH,TSH,WAS,WIW
print(",".join(forecast_counters(gs, pl2='BUB')))
# FAL,HL1,HL2,HL3,HUL,ING,INW,JAC,MOC,MUL,NHB,RUR,SCB,SHB,STK,STL,THW,TNR,WAF
print(",".join(forecast_counters(gs, pl2='DOA')))
# ANM,BRA,BRF,D2U,FAL,FUK,GOM,GRO,INW,JAC,KL3,KL4,MUL,NHB,PL2,PL3,PL4,PLA,PYP,ROR,RUR,SCB,SHB,SPM,WIW
print(",".join(forecast_counters(gs, pl2='DR4')))
# ANM,BLP,BRA,D2U,FAL,GOM,GRO,HL1,HL2,INW,JAC,LDT,MUL,NHB,PL2,ROR,RUR,SCB,SHB,TNR,WIW
print(",".join(forecast_counters(gs, pl2='HES')))
# BRA,BRF,D2U,GAD,GRO,HAW,INW,JAC,MOC,MUL,PL2,PL3,PL4,PLA,RAT,STK,THW,TRH,TSH,WAF,WAS,WIW
print(",".join(forecast_counters(gs, pl2='ILL')))
# ANM,FAL,FUK,GAD,GOM,HL2,HL3,HUL,ING,INW,MOC,MUL,PL2,PL3,PL4,PLA,POV,PYP,RAT,RUR,SCB,SPM,THA,THW,TRH,TSH,WAS
print(",".join(forecast_counters(gs, pl2='LKD')))
# ANM,BRF,D2U,FUK,GRO,HAW,HL2,HL3,HUL,ING,KL3,LDT,NHB,PL2,PL3,POV,RAT,RUR,SCB,SPM,STK,STL,THA,THW,TNR,TRH,TSH,WAS
print(",".join(forecast_counters(gs, pl2='SPG')))
# ANM,D2U,HL2,INW,JAC,KL4,PL3,PLA,POV,RUR,SCB,SHB,STL,THW,TRH,TSH,WAF
print(",".join(forecast_counters(gs, pl2='STK')))
# ANM,BLP,BRF,D2U,FAL,GAD,GRO,HL1,ING,KL3,PL4,ROR,SCB,SHB,SPM,STL,THW,TNR,TRH,TSH
print(",".join(forecast_counters(gs, pl2='TEU')))
# ANM,BRF,D2U,FAL,GAD,GOM,HUL,ING,JAC,MUL,NHB,PL2,PL3,PL4,PLA,POV,PYP,RAT,ROR,RUR,SPM,STK,STL,THA,TNR,WAS
print(",".join(forecast_counters(gs, pl2='SPG', pl1='D2U')))
# BEF,BLP,BUB,ILL
print(",".join(filter_codons("ANM,D2U,HL2,INW,JAC,KL4,PL3,PLA,POV,RUR,SCB,SHB,STL,THW,TRH,TSH,WAF".split(","), "BEF,BLP,BUB,ILL".split(","))))
# HL2,RUR
print(",".join(forecast_counters(gs, pl2='SPG', pl1='POV')))
# BUB,DOA,DR4,HES,STK
print(",".join(forecast_counters(gs, pl2='BLP', pl1='MOC')))
# BEF,DOA,DR4,LKD,SPG,STK,TEU
print(",".join(forecast_counters(gs, pl2='BLP', pl1='FUK')))
# BEF,BUB,DR4,HES,SPG,STK,TEU
print(",".join(forecast_counters(gs, pl2='BLP', pl1='HL2')))
# DOA,HES,STK,TEU
print(",".join(forecast_counters(gs, pl2='BLP', pl1='RUR')))
# HES,STK

## implement further takebacks
gs = {'pl1': ['DRA'], 'pl2': []}
counters = disp_counters(gs, "pl1", 1)

# defensive scores
tree1 = {cd: forecast_counters(gs, pl2=cd) for cd in counters}
tree2 = {cd1: [(cd2, len(forecast_counters(gs, pl2=cd1, pl1=cd2))) for cd2 in tree1[cd1]] for cd1 in counters}
scores = np.array([np.min(np.array(tree2[cd])[:,1].astype(int)) for cd in counters])
print((np.max(scores), ",".join(counters[scores == np.max(scores)])))
# (14, 'DR4,LKD')

gs['pl2'].append('DR4')
counters = disp_counters(gs, "pl2", 1)
# 1
# ANM,BEF,BLP,BRA,BUB,CAM,COC,D2U,D3P,DOA,DR2,FAL,GOM,GRO,GUG,HES,HL1,HL2,ILL,INW,JAC,LDT,LKD,LOK,LWB,MUL,NHB,PAB,PL2,ROR,RUR,SCB,SGG,SHA,SHB,TES,TEU,TNR,WIW,WOP

# offensive scores
scores = np.array([len(forecast_counters(gs, pl1=cd)) for cd in counters])
print((np.min(scores), ",".join(counters[scores == np.min(scores)])))
# (14, 'GRO,RUR')

gs['pl1'].append('GRO')
counters = disp_counters(gs, "pl1", 2)
# 2
# ANM,BLP,BRA,BUB,D2U,DR5,GOM,HL1,ILL,INW,JAC,PL4,SPG,TEU

# offensive scores
scores = np.array([len(forecast_counters(gs, pl2=cd)) for cd in counters])
print((np.min(scores), ",".join(counters[scores == np.min(scores)])))
# (15, 'D2U,JAC')

gs['pl2'].append('D2U')
counters = disp_counters(gs, "pl2", 2)
# 2
# BEF,BLP,BRA,BUB,COC,D3P,HL1,HL2,ILL,JAC,LWB,MUL,PAB,SCB,WIW

# offensive scores
scores = np.array([len(forecast_counters(gs, pl1=cd)) for cd in counters])
print((np.min(scores), ",".join(counters[scores == np.min(scores)])))
# (3, 'MUL')

gs['pl1'].append('MUL')
counters = disp_counters(gs, "pl1", 3)
# 3
# HL1,JAC,SPG

# forecasts
print(",".join(forecast_counters(gs, pl2='HL1')))
# BRA,ILL,SCB
print(",".join(forecast_counters(gs, pl2='JAC')))
# BEF,BLP,HL1,ILL,PAB,WIW
print(",".join(forecast_counters(gs, pl2='SPG')))
# BUB,COC,D3P,HL2,ILL,JAC,SCB
print(",".join(forecast_counters(gs, pl2='HL1', pl1='BRA')))
# JAC,SPG
print(",".join(forecast_counters(gs, pl2='HL1', pl1='ILL')))
#
print(",".join(forecast_counters(gs, pl2='HL1', pl1='SCB')))
# JAC


print(",".join(forecast_counters(gs, pl2='HL1', pl1='BRA', nres=3)))
# JAC,SPG,CIW,COO,DOS,GAD,HL2,HL3
print(",".join(forecast_counters(gs, pl2='HL1', pl1='ILL', nres=3)))
# CIW,DID,DOS,EXK,GAD,HL2,HL3,MOC,THW,UWY,WAS,WES
print(",".join(forecast_counters(gs, pl2='HL1', pl1='SCB', nres=3)))
# JAC,CIW,COO,DOS,GAD,HL2,HL3,LES,THW,TNR,WAS,WES
