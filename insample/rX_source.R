###
##  OLS on random X; nonlinear -- SOURCE
###


library(pracma)
library(MASS)

####
## multivariate moments of a Gaussian
####
# (powers <- rmultinom(1, 5, rep(1/4, 4))[, 1] * 2)
# mm_gaussian(powers, TRUE)
# mm_gaussian(powers, FALSE)

multinom_coeff <- function(x) {
  exp(lgamma(sum(x) + 1) - sum(lgamma(x + 1)))
}

mm_gaussian <- function(powers, naive = FALSE) {
  if (naive) {
    X <- randn(1e5, length(powers))
    for (i in 1:length(powers)) {
      X[, i] <- X[, i]^powers[i]
    }
    return(mean(apply(X, 1, prod)))
  }
  if (sum(powers %% 2 == 1) > 0) return(0)
  n <- sum(powers)/2
  logans <- sum(lgamma(powers + 1)) - sum(lgamma(powers/2 + 1)) - n * log(2)
  exp(logans)
}

evaluate_poly <- function(powers, coefs, X) {
  nbasis <- nrow(powers); n <- nrow(X)
  ans <- numeric(n)
  for (i in 1:nbasis) {
    pow <- powers[i, , drop = FALSE]
    temp <- apply(X ^ repmat(pow, n, 1), 1, prod)
    ans <- ans + coefs[i] * temp
  }
  ans
}

####
##  Get a set of powers and a basis for random polynomials
####
p <- 3
deg_dist <- rep(1/6, 6)
nbasis <- 10
lineId::zattach(rand_poly_basis(p, nbasis, deg_dist))
coefs <- nullspace %*% rnorm(dim(nullspace)[2])
t(cmat) %*% coefs
n <- 10000; X <- randn(n, p)
ff <- evaluate_poly(powers, coefs, X)
sum(ff)
t(X) %*% ff

rand_poly_basis <- function(p, nbasis, deg_dist) {
  powers <- zeros(1, p)
  ## pick some powers
  while (dim(powers)[1] < nbasis) {
    sz <- sample(1:length(deg_dist), 1, prob = deg_dist)
    newpow <- rmultinom(1, sz, prob = rep(1/p, p))[, 1]
    powers <- rbind(powers, newpow)
    powers <- unique(powers)
  }
  rownames(powers) <- paste0("d", rowSums(powers), ".", apply(powers, 1, paste0, collapse = "."))
  powers <- powers[order(rownames(powers)), ]
  ## build constraint matrix
  cmat <- matrix(NA, nbasis, p + 1)
  cmat[, 1] <- apply(powers, 1, mm_gaussian)
  for (i in 1:p) {
    powers2 <- powers; powers2[, i] <- powers2[, i] + 1
    cmat[, i + 1] <- apply(powers2, 1, mm_gaussian)
  }
  ## null space of constraint matrix
  pmat <- cmat %*% ginv(cmat)
  res <- eigen(pmat)
  nullspace <- res$vectors[, res$values < 1e-5]
  rownames(cmat) <- rownames(powers)
  list(powers = powers, cmat = cmat, nullspace = nullspace)
}
