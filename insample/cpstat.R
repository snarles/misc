####
##  Is cP biased for out-of-sample??
####

library(pracma)
library(MASS)

p <- 3
Sigma <- cov(randn(2 * p, p))
n <- 20
X <- mvrnorm(n, rep(0, p), Sigma)

## estimate insample error using Cp
cp_simulation <- function(X, bt = rnorm(ncol(X)), sigm2 = 1) {
  n <- nrow(X)
  p <- ncol(X)
  y <- X %*% bt + sqrt(sigm2) * rnorm(n)
  y2 <- X %*% bt + sqrt(sigm2) * rnorm(n)
  bth <- solve(t(X) %*% X, t(X) %*% y)
  yh <- X %*% bth
  true_sspe <- sum((yh - y2)^2) ## sum of squared prediction error
  sse <- sum((y - yh)^2)
  sigma2_hat <- sse/(n-p)
  cp_sspe <- sse + 2 * p * sigma2_hat
  c(true_sspe, cp_sspe)
}

## estimate insample error using Cp
cp_simulation_nonlin <- function(X, f, sigm2 = 1, true_sigma = FALSE) {
  n <- nrow(X)
  p <- ncol(X)
  y <- f(X) + sqrt(sigm2) * rnorm(n)
  y2 <- f(X) + sqrt(sigm2) * rnorm(n)
  bth <- solve(t(X) %*% X, t(X) %*% y)
  yh <- X %*% bth
  true_sspe <- sum((yh - y2)^2) ## sum of squared prediction error
  sse <- sum((y - yh)^2)
  sigma2_hat <- sse/(n-p)
  if (true_sigma) sigma2_hat <- sigm2
  cp_sspe <- sse + 2 * p * sigma2_hat
  c(true_sspe, cp_sspe)
}


####
##  Run the stuff
####

res <- sapply(1:1e5, function(i) cp_simulation(X))
rowMeans(res)

f <- function(X) X[, 1]^2 + sin(X[, 2]/5)
res <- sapply(1:1e5, function(i) cp_simulation_nonlin(X, f))
rowMeans(res)


f <- function(X) X[, 1]^2 + sin(X[, 2]/5)
res <- sapply(1:1e5, function(i) cp_simulation_nonlin(X, f, true_sigma = TRUE))
rowMeans(res)
