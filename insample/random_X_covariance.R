####
##  What is the covariance of beta_hat given random X and nonlinearity?
##  Without loss of generality, X ~ N(0, I) and beta = (0, 0,...)
##  Nonlinearity is generated from f(x) = a_i x^i, with E[f(X)] = 0 and E[X f(X)] = 0.
##  Hence a_0 + a_2 + 3 a_4 + 5 a_6 + ... = 0
##   and        a_1 + 3 a_3 + 5 a_5 + ... = 0
####

library(pracma)
source("insample/rX_source.R")

generate_betas <- function(n, powers, coefs, sigma2, mc.reps = 100) {
  p <- ncol(powers)
  ans <- matrix(NA, mc.reps, p)
  for (i in 1:mc.reps) {
    X <- randn(n, p)
    y <- evaluate_poly(powers, coefs, X) + sqrt(sigma2) * rnorm(n)
    bt <- solve(t(X) %*% X, t(X) %*% y)
    ans[i, ] <- bt
  }
  ans
}

## generate Betas

n <- 5
p <- 2
deg_dist <- rep(1/6, 6)
nbasis <- 5
zattach(rand_poly_basis(p, nbasis, deg_dist))
coefs <- nullspace %*% rnorm(dim(nullspace)[2])
vv <- poly_second_moment(powers, coefs)
coefs <- coefs/sqrt(vv)
sigma2 <- 1
mc.reps <- 1e4
t1 <- proc.time()
betas <- generate_betas(n, powers, coefs, sigma2, mc.reps)
proc.time() - t1
colMeans(betas)
apply(betas, 2, t.test)
cov(betas)

## approximate moments
p <- ncol(powers)
ans <- matrix(NA, mc.reps, p)
n <- 200
for (i in 1:mc.reps) {
  X <- randn(n, p)
  y <- evaluate_poly(powers, coefs, X) + sqrt(sigma2) * rnorm(n)
  actual_inv <- solve(t(X) %*% X/n)
  prox_inv <- 2 * eye(p) - t(X) %*% X/n
  f2(actual_inv, prox_inv)
  bt <- solve(t(X) %*% X, t(X) %*% y)
  ans[i, ] <- bt
}
betas <- ans
