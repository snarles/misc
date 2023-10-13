import numpy as np
import pandas as pd
import os
import scipy
from scipy.stats import rankdata

score_fns = np.array([ff for ff in os.listdir("jobs_openai/") if ff[:5]=='score'])
score_is = np.array([f.split('_')[0][6:] for f in score_fns]).astype(int)
job_is = np.unique(score_is)

simplify_map = {}
for i in range(1, 101):
    simplify_map[str(i)] = str(i)
    simplify_map["(" + str(i) + "/100)"] = str(i)
    simplify_map["(" + str(i) + ")"] = str(i)
    simplify_map["(Rating: " + str(i) + ")"] = str(i)
allowed_numbers = list(simplify_map.keys())


def simplify_st(st):
    for k, v in simplify_map.items():
        st = st.replace(k, v)
    return st

def h_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, ["deceptiveness", "shyness"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def f_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, ["empathize", "empathy"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def l_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, ["impress", "impressing", "impressiveness", "impression"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def p_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, ["passion", "desire"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def r_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, ["knowledge"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def s_finder(st):
    st = simplify_st(st)
    wds = np.array(st.replace("/", " ").replace(":", " ").replace("  ", " ").split(' '))
    wds = np.array([w.lower() for w in wds])
    if "-"  in wds:
        wds = wds[0:np.nonzero(wds == '-')[0][0]]
    kwd_hits = np.isin(wds, [
        "efficiency","reactvity", "learning",
        "intelligence"])
    num_hits = np.isin(wds, allowed_numbers)
    if np.sum(num_hits) > 0 and np.sum(kwd_hits) > 0:
        return wds[num_hits][0]
    else:
        return "FAIL"

def extract_numbers(txt):
    hs = np.unique([h_finder(st) for st in txt])
    fs = np.unique([f_finder(st) for st in txt])
    ls = np.unique([l_finder(st) for st in txt])
    ps = np.unique([p_finder(st) for st in txt])
    rs = np.unique([r_finder(st) for st in txt])
    ss = np.unique([s_finder(st) for st in txt])
    assert(np.sum(hs != "FAIL")==1)
    assert(np.sum(fs!= "FAIL")==1)
    assert(np.sum(ls != "FAIL")==1)
    assert(np.sum(ps != "FAIL")==1)
    assert(np.sum(rs != "FAIL")==1)
    assert(np.sum(ss != "FAIL")==1)
    v = np.array([
        hs[hs != "FAIL"][0],
        fs[fs != "FAIL"][0],
        ls[ls != "FAIL"][0],
        ps[ps!= "FAIL"][0],
        rs[rs != "FAIL"][0],
        ss[ss != "FAIL"][0],
        ])
    v = np.array([simplify_map[x] for x in v]).astype(int)
    return v



job_vecs = []
job_final_is = []

for ii in job_is:
    fns = score_fns[score_is == ii]
    rep_is = np.array([f.split('rep')[1].split('.txt')[0] for f in fns]).astype(int)
    vs = []
    for jj in rep_is:
        fn = 'scores%i_rep%i.txt' % (ii, jj)
        txt = np.loadtxt('jobs_openai/' + fn, dtype = str, delimiter = '@')
        if txt.size > 2:
            try:
                v = extract_numbers(txt)
                vs.append(v)
            except:
                0
                #print(txt)
                #print(" ")
    if len(vs) > 1:
        vs = np.array(vs)
        vv = np.mean(vs, axis = 0)
        job_final_is.append(ii)
        job_vecs.append(vv)

job_names = np.loadtxt('wikilist_jobs.txt', dtype=str, delimiter = ':')

jobs_final = job_names[job_final_is]
np.savetxt("job_open_ai_names.txt", dishes_final, fmt = "%s")

arr = np.array(job_vecs)
np.savetxt("job_open_ai_vec.txt", arr)
for i in range(6):
    arr[:, i] = rankdata(arr[:, i])


for i in range(len(jobs_final)):
    st = jobs_final[i].replace(',', '-') + ","
    v = arr[i]
    v = v/np.sum(v) * 20
    v = np.floor(v).astype(int)
    for j in range(6):
        st = st + str(v[j]) + ','
    print(st[:-1])
