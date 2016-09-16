library(pracma)

# compute_true_Vs <- function(X, mu, ss) {
#   Xmu <- as.numeric(t(X) %*% mu)
#   XtX <- t(X) %*% X
#   ans <- apply(ss, 2, function(S) {
#     S <- S[S != 0]
#     as.numeric(t(Xmu[S]) %*% solve(XtX[S, S], Xmu[S]))
#   })
#   names(ans) <- apply(ss, 2, function(S) {
#     S <- S[S != 0]
#     paste(S, collapse = ".")
#   })
#   ans
# }
# 
# compute_true_Vs2 <- function(X, mu, ss) {
#   ans <- apply(ss, 2, function(S) {
#     res <- qr(X[, S])
#     qr.Q(res)
#     sum((t(qr.Q(res)) %*% mu)^2)
#   })
#   names(ans) <- apply(ss, 2, function(S) {
#     S <- S[S != 0]
#     paste(S, collapse = ".")
#   })
#   ans
# }

## empirically compute true null distribution
compute_true_Vs2_par <- function(X, mus, ss) {
  ans <- apply(ss, 2, function(S) {
    res <- qr(X[, S])
    qr.Q(res)
    colSums((t(qr.Q(res)) %*% mus)^2)
  })
  colnames(ans) <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    paste(S, collapse = ".")
  })
  ans
}

normalize <- function(x) {
  x/sqrt(sum(x^2))
}


####
##  Meta-calculations to check accuracy of IU comps
####

approx_probs <- function(Vss, thres, max.order = 20) {
  counts <- apply(Vss, 1, function(v) sum(v > thres))
  (true_prob <- mean(counts > 0))
  tab <- table(counts)/length(counts)
  ns <- as.numeric(names(tab))
  proxes <- numeric()
  for (i in 1:max.order) {
    current <- (-1)^(i+1) * sum(choose(ns, i) * tab)
    proxes <- c(proxes, current)
  }
  names(proxes) <- paste(1:max.order)
  c(true_prob = true_prob, cumsum(proxes))
}

n <- 1000
p <- 30
X <- randn(n, p)
X <- apply(X, 2, normalize)
k <- 3
ss1 <- combn(1:p, 1)
ss2 <- combn(1:p, 2)
ss3 <- combn(1:p, 3)


# y <- normalize(rnorm(n))
# 
# Vs <- compute_true_Vs(X, y, ss)
# Vs2 <- compute_true_Vs2(X, y, ss)

n.its <- 1e3
ys <- randn(n, n.its)
ys <- apply(ys, 2, normalize)
# Vs <- compute_true_Vs2(X, ys[, 1], ss)
Vss1 <- compute_true_Vs2_par(X, ys, ss1)
Vss2 <- compute_true_Vs2_par(X, ys, ss2)
Vss3 <- compute_true_Vs2_par(X, ys, ss3)


dist1 <- apply(Vss1, 1, max)
hist(dist1)
(thres1 <- quantile(dist1, 0.99))
max(dist1)

dist2 <- apply(Vss2, 1, max)
max(dist2)
max(dist2[dist1 < thres1])
(thres2 <- quantile(dist2[dist1 < thres1], 0.99))
(thres2_orig <- quantile(dist2, 0.99))
counts2_orig <- apply(Vss2, 1, function(v) sum(v > thres2_orig))
counts2 <- apply(Vss2[dist1 < thres1, ], 1, function(v) sum(v > thres2))

barplot(table(counts2_orig)[-1])
barplot(table(counts2)[-1])

approx_probs(Vss2, thres2_orig, max.order = 5)
approx_probs(Vss2[dist1 < thres1, ], thres2, max.order = 5)


dist3 <- apply(Vss3, 1, max)
max(dist3)
max(dist3[dist2 < thres2])
(thres3 <- quantile(dist3[dist2 < thres2], 0.99))
(thres3_orig <- quantile(dist3, 0.99))
counts3_orig <- apply(Vss3, 1, function(v) sum(v > thres3_orig))
counts3 <- apply(Vss3[dist2 < thres2, ], 1, function(v) sum(v > thres3))

barplot(table(counts3_orig)[-1])
barplot(table(counts3)[-1])

approx_probs(Vss3, thres3_orig, max.order = 5)
approx_probs(Vss3[dist2 < thres2, ], thres3, max.order = 5)



# sup_dist <- apply(Vss, 1, max)
# hist(sup_dist)
# # mean(sup_dist > 0.085)
# # 
# # ## probability of an intersection event
# max(sup_dist)
# thres <- 0.025
# counts <- apply(Vss, 1, function(v) sum(v > thres))
# barplot(table(counts)[-1])
# approx_probs(Vss, thres, max.order = 5)
