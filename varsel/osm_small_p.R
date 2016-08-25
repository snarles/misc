library(pracma)

compute_true_Vs <- function(X, mu, ss) {
  Xmu <- as.numeric(t(X) %*% mu)
  XtX <- t(X) %*% X
  ans <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    as.numeric(t(Xmu[S]) %*% solve(XtX[S, S], Xmu[S]))
  })
  names(ans) <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    paste(S, collapse = ".")
  })
  ans
}

n <- 20
p <- 5
k0 <- 2
k <- 2
sst <- 2
mut <- 0.1
sigma2 <- 1
X <- randn(n, p)
bt0 <- numeric(p)
bt0[sample(p, k0)] <- sst * rnorm(k0)
mu <- mut * rnorm(n) + X %*% bt0
get_Y <- function() mu + sqrt(sigma2) * rnorm(n)

ss <- combn(1:p, k)
Vs_true <- compute_true_Vs(X, mu, ss)
Vs_true


