####
##  Is cP biased for out-of-sample??
####

library(pracma)
library(MASS)

p <- 3
Sigma <- cov(randn(2 * p, p))
n <- 20
X <- mvrnorm(n, rep(0, p), Sigma)
X2 <- mvrnorm(n, rep(0, p), Sigma)


## estimate insample error using Cp
cp_simulation <- function(X, bt = rnorm(ncol(X)), sigm2 = 1,
                          true_sigma = FALSE) {
  n <- nrow(X)
  p <- ncol(X)
  y <- X %*% bt + sqrt(sigm2) * rnorm(n)
  y2 <- X %*% bt + sqrt(sigm2) * rnorm(n)
  bth <- solve(t(X) %*% X, t(X) %*% y)
  yh <- X %*% bth
  true_sspe <- sum((yh - y2)^2) ## sum of squared prediction error
  sse <- sum((y - yh)^2)
  sigma2_hat <- sse/(n-p)
  if (true_sigma) sigma2_hat <- sigm2
  cp_sspe <- sse + 2 * p * sigma2_hat
  c(true = true_sspe, est = cp_sspe)
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
  c(true = true_sspe, est = cp_sspe)
}

## estimate outsample error using Cp
cp_simulation_out <- function(X, X2, bt = rnorm(ncol(X)), sigm2 = 1, 
                              true_sigma = FALSE) {
  n <- nrow(X)
  p <- ncol(X)
  y <- X %*% bt + sqrt(sigm2) * rnorm(n)
  y2 <- X2 %*% bt + sqrt(sigm2) * rnorm(n)
  bth <- solve(t(X) %*% X, t(X) %*% y)
  yh <- X %*% bt
  yh2 <- X2 %*% bth
  true_sspe <- sum((yh2 - y2)^2) ## sum of squared prediction error
  sse <- sum((y - yh)^2)
  sigma2_hat <- sse/(n-p)
  if (true_sigma) sigma2_hat <- sigm2
  cp_sspe <- sse + 2 * p * sigma2_hat
  c(true = true_sspe, est = cp_sspe)
}

####
##  Run the stuff
####

res_cp <- sapply(1:1e5, function(i) cp_simulation(X, rep(0,p)))
rowMeans(res_cp)

f <- function(X) X[, 1]^2 + sin(X[, 2]/5)
res_cp_nl <- sapply(1:1e5, function(i) cp_simulation_nonlin(X, f))
rowMeans(res_cp_nl)
res_cp_nl2 <- sapply(1:1e5, function(i) cp_simulation_nonlin(X, f, true_sigma = TRUE))
rowMeans(res_cp_nl2)



res_out <- sapply(1:1e5, function(i) {
  X <- mvrnorm(n, rep(0, p), Sigma)
  X2 <- mvrnorm(n, rep(0, p), Sigma)
  cp_simulation_out(X, X2 , true_sigma = TRUE)
  }
)
rowMeans(res_out)

res_out_sameSig <- sapply(1:1e5, function(i) {
  X <- mvrnorm(n, rep(0, p), Sigma)
  X2 <- mvrnorm(n, rep(0, p), Sigma)
  
  cp_simulation_out(X, X2 , true_sigma = TRUE)
}
)
rowMeans(res_out_sameSig)
