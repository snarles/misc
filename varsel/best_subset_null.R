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
##  Intersection-union computations
####

## probability that min R^2 over subsets in ss > thres
subspace_intersection_prob <- function(X, ss, thres) {
  
}

## probability that max over ss subsets will yield R^2 > thres
full_int_union <- function(X, ss, thres) {
  
}



n <- 30
p <- 8
X <- randn(n, p)
X <- apply(X, 2, normalize)
k <- 3
ss <- combn(1:p, k)



# y <- normalize(rnorm(n))
# 
# Vs <- compute_true_Vs(X, y, ss)
# Vs2 <- compute_true_Vs2(X, y, ss)

n.its <- 20000
ys <- randn(n, n.its)
ys <- apply(ys, 2, normalize)
# Vs <- compute_true_Vs2(X, ys[, 1], ss)
Vss <- compute_true_Vs2_par(X, ys, ss)

sup_dist <- apply(Vss, 1, max)
hist(sup_dist)
mean(sup_dist > 0.4)

