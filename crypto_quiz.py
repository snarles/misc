import twl
import numpy as np
import numpy.random as npr
import string

def enumwords(depth):
    words = []
    stems = [['']] + [[] for i in range(depth)]
    for l in range(depth):
        for stem in stems[l]:
            chs = twl.children(stem)
            for c in chs:
                if c == '$':
                    words.append(stem)
                else:
                    stems[l+1].append(stem + c)
    return words, stems

words, stems = enumwords(7)

codestring = raw_input("enter code: ")
code = [s for s in codestring]
code0 = [s for s in string.letters[:26]]
codebook = {i : j for (i,j) in zip(code0, code)}
print(codebook)
npr.shuffle(words)

ntrials = input("number of trials: ")
for i in range(ntrials):
    word = words[i]
    solution = ''.join([codebook[s] for s in word])
    flag = True
    while flag:
        answer = raw_input(word + ': ')
        if answer==solution:
            flag = False
