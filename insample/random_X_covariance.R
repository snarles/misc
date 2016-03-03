####
##  What is the covariance of beta_hat given random X and nonlinearity?
##  Without loss of generality, X ~ N(0, I) and beta = (0, 0,...)
##  Nonlinearity is generated from f(x) = a_i x^i, with E[f(X)] = 0 and E[X f(X)] = 0.
##  Hence a_0 + a_2 + 3 a_4 + 5 a_6 + ... = 0
##   and        a_1 + 3 a_3 + 5 a_5 + ... = 0
####

library(pracma)

generate_betas <- function(n, p, sigma, mc.reps = 100) {
  ans <- matrix(NA, mc.reps, p)
  for (i in 1:mc.reps) {
    X <- randn(n, p)
    y <- X[, 1] + rnorm(n)
    bt <- solve(t(X) %*% X, t(X) %*% y)
    ans[i, ] <- bt
  }
  ans
}

n <- 20
p <- 4
betas <- generate_betas(n, p, sigma, mc.reps)

