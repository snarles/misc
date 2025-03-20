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

kee="ANM,BLP,BLW,BUB,CAA,CAM,DOS,DRA,FAL,GAM,GRO,HAW,HUL,ING,IRM,LOK,ROR,SHU,SPG,SPM,STL,THA,THO,WAS".split(",")
goa="AVE,BEF,BRA,BRF,CIW,COC,COC,COO,DBM,FAD,FUK,GAD,GOA,GUG,HOH,ILL,INW,MAM,MEG,MUL,POV,PYP,SCB,SGG,SHA,SHB,TES,WAF,WES,WIW,WOP".split(",")
act="ARE,COC,D2U,D3P,DID,EG1,EXK,JAC,LDT,LES,LWB,RAT,RUR,STK,TEU,TNR,TRH,TRK,TSH,UWY,ZAC".split(",")
ner="DOA,DR2,DR3,DR4,DR5,GOM,GOW,HES,HL1,HL2,HL3,KL3,KL4,LKD,MOC,NHB,PAB,PL2,PL3,PL4,PLA,SPS,THW".split(",")
