####
##  How do the effects of nonlinearity differ from heteroskedasticity?
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

generate_betasH <- function(n, powers, coefs, sigma2, mc.reps = 100) {
  p <- ncol(powers)
  ans <- matrix(NA, mc.reps, p)
  for (i in 1:mc.reps) {
    X <- randn(n, p)
    varf <- evaluate_poly(powers, coefs, X)^2 + sigma2
    y <- sqrt(varf) * rnorm(n)
    bt <- solve(t(X) %*% X, t(X) %*% y)
    ans[i, ] <- bt
  }
  ans
}

## generate poly

n <- 20
p <- 1
deg_dist <- rep(1/3, 3)
nbasis <- 3
fvar <- 1
zattach(rand_poly_basis(p, nbasis, deg_dist))
coefs <- nullspace %*% rnorm(dim(nullspace)[2])
vv <- poly_second_moment(powers, coefs)
coefs <- sqrt(fvar) * coefs/sqrt(vv)
sigma2 <- 1
mc.reps <- 1e4

## linear case
betas_l <- generate_betas(n, powers, 0 * coefs, sigma2 + fvar, mc.reps)

## nonlinear case
betas_nl <- generate_betas(n, powers, coefs, sigma2, mc.reps)

## linear hetero case
betas_h <- generate_betasH(n, powers, coefs, sigma2, mc.reps)


colMeans(betas_l)
colMeans(betas_nl)
colMeans(betas_h)

cov(betas_l)
cov(betas_nl)
cov(betas_h)