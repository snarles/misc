import twl
import numpy as np
import numpy.random as npr
import string

words = list(twl.iterator())

codestring = raw_input("enter code: ")
code = [s for s in codestring]
code0 = [s for s in string.letters[:26]]
codebook = {i : j for (i,j) in zip(code0, code)}
print(codebook)
npr.shuffle(words)

ntrials = input("number of encoding trials: ")
for i in range(ntrials):
    word = words[i]
    solution = ''.join([codebook[s] for s in word])
    flag = True
    while flag:
        answer = raw_input(word + ': ')
        if answer==solution:
            flag = False

ntrials = input("number of decoding trials: ")
for i in range(ntrials, 2*ntrials):
    word = words[i]
    solution = ''.join([codebook[s] for s in word])
    flag = True
    while flag:
        answer = raw_input(solution + ': ')
        if answer==word:
            flag = False

