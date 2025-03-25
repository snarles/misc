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

def forecast_counters(gs, pl1='', pl2=''):
    p1c = gs['pl1']
    p2c = gs['pl2']
    if pl1 != '':
        p1c = np.concatenate((p1c, [pl1]))
    if pl2 != '':
        p2c = np.concatenate((p2c, [pl2]))
    if len(p1c) == len(p2c):
        curr = 'pl2'
    else:
        curr = 'pl1'
    nb = disp_nbeat({'pl1':p1c, 'pl2':p2c}, curr, verbose=False)
    if nb <= 1:
        counters = ucodons
    else:
        counters = disp_counters({'pl1':p1c, 'pl2':p2c}, curr, nb-1, verbose=False)
    return counters

## Game 3

#     P1         P2 (goes first)
#     - pre-draft -
# p1- DRA  (0)   CAA  (0)
# p2- COO  (0)   HES  (2)
# p3- DR5  (1)   BEF  (3)
# p4- COCu (3)   THW  (3)
# p5- IRM  (4)   CAM  (3)
# p6- PLA  (4)   DOS  (4)
#  1- SPM  (3)   GOM  (4)
#  2-      ( )        ( )
# [Takebacks]

gs = {'pl1': ['DRA'], 'pl2': []}
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))

## find good counters for DR?...

cands = ucodons[all_doubs[2]=="DR"]
score = np.array([np.sum([winner(cd, cand)==1 for cand in cands]) for cd in ucodons])
print(ucodons[score == np.max(score)])
print(ucodons[score == np.max(score)-1])
# ['CAM' 'FAL' 'LOK' 'MUL']
# ['BEF' 'BLP' 'BRA' 'BUB' 'COC' 'D2U' 'D3P' 'DOA' 'GRO' 'HES' 'HL1' 'JAC' 'NHB' 'ROR' 'RUR' 'SHA' 'SHB' 'STK' 'TES' 'TEU' 'WIW' 'WOP']

gs['pl2'].append('CAA')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))

## find best complement for DRA

score = np.array([len(filter_codons(ucodons, ['DRA', cd])) for cd in ucodons])
print(ucodons[score == np.min(score)])
# ['COO']
gs['pl1'].append('COO')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))

gs['pl2'].append('HES')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))

gs['pl1'].append('DR5')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))

gs['pl2'].append('BEF')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))
# PL2 scores 3 with BEF
# 3
# CAM,CIW,COC,GOW,ILL,IRM,MAM,MUL,RAT,SHU,TRK,WAS
# 2
# BRF,BUB,GAD,GRO,HAW,INW,PL2,PL4,PLA,SPG,WES

gs['pl1'].append('COC')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))
# PL1 scores 3 with COC
# 3
# DID,GAM,GOW,SHU,THW
# 2
# AVE,CAM,COO,DOS,FAL,GUG,HL1,KL4,LOK,LWB,PAB,PL4,SCB,SGG,SPM,SPS

# offensive picking
score = np.array([len(forecast_counters(gs, pl2=cd)) for cd in counters])
print([(a,b) for a,b in np.array(list(zip(counters, score)))[np.argsort(score)[:5]]])
# [('THW', '22'), ('SHU', '24'), ('GAM', '26'), ('DID', '31'), ('GOW', '35')]

gs['pl2'].append('THW')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))
# PL2 scores 3 with THW
# 4
# GOW,IRM,SHU
# 3
# HAW,PL4,PLA,WES
# 2
# ARE,AVE,DR3,GOA,HL1,HL2,LDT,NHB,PAB,PYP,ROR,RUR,SHA,UWY,WOP

gs['pl1'].append('IRM')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))
# PL1 scores 4 with IRM
# 3
# AVE,CAM,DOS,KL4,SCB,SGG,SPM

# offensive picking
score = np.array([len(forecast_counters(gs, pl2=cd)) for cd in counters])
print([(a,b) for a,b in np.array(list(zip(counters, score)))[np.argsort(score)[:5]]])
# [('CAM', '20'), ('SPM', '20'), ('DOS', '21'), ('KL4', '22'), ('SCB', '22')]

gs['pl2'].append('CAM')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))
# PL2 scores 3 with CAM
# 4
# HAW,PL4,PLA
# 3
# HL1,HL2,LDT,NHB,PYP,ROR,RUR
# 2
# ANM,BRA,D2U,ING,PL3,SPM,STL,THA,WAF,WIW
print(gs)
# {'pl1': ['DRA', 'COO', 'DR5', 'COC', 'IRM'], 'pl2': ['CAA', 'HES', 'BEF', 'THW', 'CAM']}

# offensive picking
score = np.array([len(forecast_counters(gs, pl1=cd)) for cd in counters])
print([(a,b) for a,b in np.array(list(zip(counters, score)))[np.argsort(score)[:5]]])
# [('HAW', '11'), ('PL4', '11'), ('PLA', '15'), ('NHB', '17'), ('ROR', '18')]

gs['pl1'].append('PLA')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))
# PL1 scores 4 with PLA
# 4
# DOS,SGG,SPM
# 3
# BLP,BRF,DBM,DR5,GAD,GOM,GRO,NHB,PYP,SHB,THO,WOP

# forecasts
print(",".join(forecast_counters(gs, pl2='DOS')))
print(",".join(forecast_counters(gs, pl2='SGG')))
print(",".join(forecast_counters(gs, pl2='SPM')))
# LDT,NHB,RUR,ANM,SPM,THA,WIW
# PL4,HL1,LDT,NHB,ROR,RUR,BRA,D2U,ING,THA,WAF,WIW
# HAW,HL2,PYP,ROR,BRA,ING,PL3,THA

gs['pl2'].append('DOS')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))
# PL2 scores 4 with DOS
# 4
# LDT,NHB,RUR
# 3
# ANM,SPM,THA,WIW

#     P1         P2 (goes first)
#     - pre-draft -
# p1- DRA  (0)   CAA  (0)
# p2- COO  (0)   HES  (2)
# p3- DR5  (1)   BEF  (3)
# p4- COCu (3)   THW  (3)
# p5- IRM  (4)   CAM  (3)
# p6- PLA  (4)   DOS  (4)

# forecasts
print(",".join(forecast_counters(gs, pl1='LDT')))
# SPM,BLP,DBM,GOM,GRO,PYP,SHB,BRA,BUB,DRA,MAM,RAT,STK,TNR
print(",".join(forecast_counters(gs, pl1='RUR')))
# SPM,DR5,GAD,GRO,THO,MUL,STK,TNR,ZAC
print(",".join(forecast_counters(gs, pl1='ANM')))
# SGG,SPM,BLP,DR5,NHB,PYP,SHB,THO,BUB,EXK,MUL,RAT,TNR,CAA,COO,D2U,EG1,FAD,HAW,HOH,IRM,JAC,LDT,LES,MEG,PL4,RUR
print(",".join(forecast_counters(gs, pl1='SPM')))
# SGG,BLP,BRF,GAD,GOM,PYP,SHB,BRA,BUB,DR4,DRA,EXK,MAM,MUL,TNR,ZAC,CAA,COO,EG1,FAD,HAW,HOH,INW,JAC,LES,MEG,SHA,SPS
print(",".join(forecast_counters(gs, pl1='THA')))
# BLP,BRF,DBM,DR5,GAD,GRO,PYP,SHB,THO,WOP,BRA,BUB,DR4,DRA,EXK,MOC,RAT,STK,ZAC,CAA,D2U,FAD,HAW,HL1,IRM,JAC,LDT,LES,MEG,PL4,RUR,SHA

gs['pl1'].append('SPM')
nb = disp_nbeat(gs, "pl1")
counters = disp_counters(gs, "pl1", max(0, nb-1))
# PL1 scores 3 with SPM
# 5
# SGG
# 4
# BLP,BRF,GAD,GOM,PYP,SHB
# 3
# BRA,BUB,DR4,DRA,EXK,MAM,MUL,TNR,ZAC
# 2
# CAA,COO,EG1,FAD,HAW,HOH,INW,JAC,LES,MEG,SHA,SPS

# forecasts
print(",".join(forecast_counters(gs, pl2='SGG')))
# LDT,NHB,RUR,THA,WIW
print(",".join(forecast_counters(gs, pl2='BLP')))
# NHB,RUR,WIW,HUL,MOC,POV,RAT,SHB,TNR,WAS
print(",".join(forecast_counters(gs, pl2='GOM')))
# NHB,RUR,ANM,THA,COO,INW,MOC,SHB

gs['pl2'].append('GOM')
nb = disp_nbeat(gs, "pl2")
counters = disp_counters(gs, "pl2", max(0, nb-1))
# PL2 scores 4 with GOM
# 5
# NHB,RUR
# 4
# ANM,THA
# 3
# COO,INW,MOC,SHB

# forecasts
print(",".join(forecast_counters(gs, pl1='RUR')))
# GAD,MUL,TNR,ZAC
print(",".join(forecast_counters(gs, pl1='ANM')))
# SGG,BLP,PYP,SHB,BUB,EXK,MUL,TNR,CAA,COO,EG1,FAD,HAW,HOH,JAC,LES,MEG
print(",".join(forecast_counters(gs, pl1='THA')))
# BLP,BRF,GAD,PYP,SHB,BRA,BUB,DR4,DRA,EXK,ZAC,CAA,FAD,HAW,JAC,LES,MEG,SHA
