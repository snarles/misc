import numpy as np

seq1 = np.arange(1, 36)
seq5 = np.concatenate([np.arange(0, 7) * 5 + i + 1 for i in range(5)])
seq7 = np.concatenate([np.arange(0, 5) * 7 + i + 1 for i in range(7)])
idx5 = { seq5[i] : i + 1 for i in range(35)}
idx7 = { seq7[i] : i + 1 for i in range(35)}

def quality(seq):
    ss1 = np.concatenate([seq, [seq[0] + 35]])
    s5 = np.sort([idx5[s] for s in seq])
    s7 = np.sort([idx7[s] for s in seq])
    ss5 = np.concatenate([s5, [s5[0] + 35]])
    ss7 = np.concatenate([s7, [s7[0] + 35]])
    sc1 = np.max(np.diff(ss1))
    sc5 = np.max(np.diff(ss5))
    sc7 = np.max(np.diff(ss7))
    return np.max([sc1, sc5, sc7])

# bisection search
for i in range(1, 35):
    for j in range(i+1, 36):
        q = quality([i, j])
        if q < 19:
            print((i, j, q))

# trisection search
for i in range(1, 34):
    for j in range(i+1, 35):
        for k in range(j+1, 36):
            q = quality([i, j, k])
            if q < 17:
                print((i, j, k, q))

# (1, 12, 24, 16) (1, 13, 24, 16) (1, 17, 33, 16)
# (2, 13, 25, 16) (2, 14, 25, 16) (2, 18, 34, 16)
# (3, 15, 26, 16) (3, 19, 35, 16) (4, 15, 26, 16)
# (5, 16, 27, 16) (5, 16, 28, 16) (6, 17, 29, 16) (6, 18, 29, 16)
# (7, 18, 30, 16) (7, 19, 30, 16) (8, 20, 31, 16) (9, 20, 31, 16)
# (10, 21, 32, 16) (10, 21, 33, 16) (11, 22, 34, 16) (11, 23, 34, 16)
# (12, 23, 35, 16) (12, 24, 35, 16)

# quadrisection search
for i in range(1, 33):
    for j in range(i+1, 34):
        for k in range(j+1, 35):
            for l in range(k+1, 36):
                q = quality([i, j, k, l])
                if q < 13:
                    print((i, j, k, l, q))

# (1, 10, 18, 26, 12) (1, 10, 18, 27, 12) (1, 10, 19, 27, 12)
# (2, 11, 20, 28, 12) (3, 11, 20, 28, 12) (3, 11, 20, 29, 12)
# (4, 12, 21, 30, 12) (4, 13, 21, 30, 12) (5, 13, 21, 30, 12)
# (6, 15, 23, 31, 12) (6, 15, 23, 32, 12) (6, 15, 24, 32, 12)
# (7, 16, 25, 33, 12) (8, 16, 25, 33, 12) (8, 16, 25, 34, 12)
# (9, 17, 26, 35, 12) (9, 18, 26, 35, 12) (10, 18, 26, 35, 12)

# quinquesection search
for i in range(1, 32):
    for j in range(i+1, 33):
        for k in range(j+1, 34):
            for l in range(k+1, 35):
                for m in range(l+1, 36):
                    q = quality([i, j, k, l, m])
                    if q < 11:
                        print((i, j, k, l, m, q))

# (1, 5, 9, 17, 27, 10) (1, 5, 13, 23, 31, 10) (1, 9, 17, 25, 27, 10)
# (2, 6, 10, 18, 28, 10) (3, 13, 21, 25, 29, 10) (4, 6, 14, 22, 30, 10)
# (4, 14, 22, 26, 30, 10) (5, 13, 23, 31, 35, 10) (6, 10, 14, 22, 32, 10)
# (6, 14, 22, 30, 32, 10) (7, 11, 15, 23, 33, 10) (8, 18, 26, 30, 34, 10)
# (9, 11, 19, 27, 35, 10) (9, 19, 27, 31, 35, 10)

# sexisection search
for i in range(1, 31):
    for j in range(i+1, 32):
        for k in range(j+1, 33):
            for l in range(k+1, 34):
                for m in range(l+1, 35):
                    for n in range(m+1, 36):
                        q = quality([i, j, k, l, m, n])
                        if q < 10:
                            print((i, j, k, l, m, n, q))

# (too many to list)

# septisection search
for i in range(1, 10):
    for j in range(i+1, 15):
        for k in range(j+1, 20):
            for l in range(k+1, 25):
                for m in range(l+1, 30):
                    for n in range(m+1, 36):
                        for o in range(n+1, 36):
                            q = quality([i, j, k, l, m, n, o])
                            if q < 9:
                                print((i, j, k, l, m, n, o, q))

# octisection search
for i in range(1, 8):
    for j in range(i+3, 13):
        for k in range(j+3, 17):
            for l in range(k+3, 21):
                for m in range(l+3, 25):
                    for n in range(m+3, 29):
                        for o in range(n+3, 33):
                            for p in range(o+3, 36):
                                q = quality([i, j, k, l, m, n, o, p])
                                if q < 7:
                                    print((i, j, k, l, m, n, o, p, q))

# (1, 5, 9, 13, 17, 21, 25, 30, 6)
# (1, 5, 9, 13, 17, 21, 25, 31, 6)
# (1, 6, 10, 14, 18, 22, 26, 30, 6)
# (1, 7, 11, 15, 19, 23, 27, 31, 6)
# (2, 6, 10, 15, 20, 24, 28, 32, 6)
# (3, 7, 11, 15, 21, 25, 29, 33, 6)
# (4, 8, 12, 16, 21, 26, 30, 34, 6)
# (6, 10, 14, 18, 22, 26, 30, 35, 6)
# (6, 11, 15, 19, 23, 27, 31, 35, 6)

# [no results with quality <= 5 for nonisection]

# decisection search
for i in range(1, 6):
    for j in range(i+2, 9):
        for k in range(j+2, 12):
            for l in range(k+2, 15):
                for m in range(l+2, 18):
                    for n in range(m+2, 23):
                        for o in range(n+2, 28):
                            for p in range(o+2, 31):
                                for r in range(p+2, 34):
                                    for s in range(r+2, 36):
                                        q = quality([i, j, k, l, m, n, o, p, r, s])
                                        if q < 6:
                                            print((i, j, k, l, m, n, o, p, r, s, q))

# (1, 3, 5, 9, 14, 18, 20, 22, 26, 31, 5)
# (1, 3, 7, 12, 16, 18, 20, 24, 29, 33, 5)
# (1, 4, 7, 10, 15, 20, 23, 26, 29, 32, 5)
# (1, 4, 7, 12, 17, 20, 23, 26, 29, 32, 5)
# (1, 4, 9, 14, 17, 20, 23, 26, 29, 32, 5)
# (1, 6, 11, 14, 17, 20, 23, 26, 29, 32, 5)
# (2, 5, 8, 11, 14, 17, 20, 25, 30, 33, 5)
# (2, 5, 8, 11, 14, 17, 22, 27, 30, 33, 5)
# (2, 5, 8, 11, 14, 19, 24, 27, 30, 33, 5)
# (2, 5, 8, 11, 16, 21, 24, 27, 30, 33, 5)
# (2, 7, 11, 13, 15, 19, 24, 28, 30, 32, 5)
# (3, 6, 9, 12, 15, 20, 25, 28, 31, 34, 5)
# (3, 6, 9, 12, 17, 22, 25, 28, 31, 34, 5)
# (4, 6, 8, 12, 17, 21, 23, 25, 29, 34, 5)
# (4, 7, 10, 13, 16, 19, 22, 25, 30, 35, 5)
# (4, 7, 10, 13, 16, 19, 22, 27, 32, 35, 5)
# (4, 7, 10, 13, 16, 19, 24, 29, 32, 35, 5)
# (4, 7, 10, 13, 16, 21, 26, 29, 32, 35, 5)
