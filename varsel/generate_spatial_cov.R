## generate a spatial covariance structure

library(pracma)
library(MASS)

gen_Sigma <- function(p, d, h) {
  xs <- randn(p, d)
  dd <- pdist(xs)
  Sigma <- exp(-dd/(h^2))
  Sigma
}

gen_Xy <- function(Sigma, bt, n, sigma2) {
  p <- nrow(Sigma)
  X <- mvrnorm(n, rep(0, p), Sigma)
  mu <- X %*% bt
  eps <- sqrt(sigma2) * rnorm(n)
  y <- mu + eps
  list(X = X, y = y)
}