####
##  What is the covariance of beta_hat given random X?
##  
####

library(pracma)
source("insample/rX_source.R")
n <- 10
p <- 3
deg_dist <- rep(1/6, 6)
nbasis <- 5
zattach(rand_poly_basis(p, nbasis, deg_dist))
coefs <- nullspace %*% rnorm(dim(nullspace)[2])
vv <- poly_second_moment(powers, coefs)
coefs <- coefs/sqrt(vv)
sigma2 <- 1
X <- randn(n, p)
y <- evaluate_poly(powers, coefs, X) + sqrt(sigma2) * rnorm(n)

## orthogonalize X

zattach(orthogonalize_X(X, y))
# ginv(X) %*% y
# tmat %*% ginv(Xtilde) %*% y

## bootstrap betas

boot.reps <- 5000
boot.betas <- matrix(NA, boot.reps, p)
for (i in 1:boot.reps) {
  inds <- sample(n, n, TRUE)
  boot.betas[i, ] <- (MASS::ginv(Xtilde[inds, ]) %*% y[inds])
}
boot.mu <- colMeans(boot.betas)
bt <- (MASS::ginv(Xtilde) %*% y)[, 1]
boot.cov <- cov(boot.betas)
samp.cov <- solve(t(Xtilde) %*% Xtilde) * sqrt(f2(Xtilde %*% bt - y)/(n-1))

## compare inv(XtX) to taylor approximation

inds <- sample(n, n, TRUE)
Xnew <- Xtilde[inds, ]; ynew <- y[inds]
solve(t(Xnew) %*% Xnew/n)
2 * eye(p) - t(Xnew) %*% Xnew/n

## approximate the bias
(boot.bias <- boot.mu - bt)
bterms <- matrix(NA, n, p)
for (i in 1:n) {
  xi <- Xtilde[i, ]; yi <- y[i]
  bterms[i, ] <- f2(xi) * xi * yi - xi * yi
}
colMeans(bterms)/n

