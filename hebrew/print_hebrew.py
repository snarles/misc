import pandas as pd
import numpy as np
from numpy.random import default_rng
rng = default_rng()

words = pd.read_csv("words500.txt")
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

for ind in range(len(words)):
    print(word2heb(words.loc[ind, "word"]))
    if ind % 20 == 0:
        input()
