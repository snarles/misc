############
## Estimation in the linear/linear case
############
library(class)
library(parallel)

isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

sqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

cosmat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
}

bt <- diag(c(1, 2))
bth <- cosmat(0.01) %*% diag(c(1.01, 1.99))
bth
var_eps <- 1
cov_x <- diag(rep(1, 2))
seed <- 1
scov_x <- sqrtm(cov_x)
k_cl <- 4
n_trials <- 10
mc.cores <- 25

simulate0 <- function(bt, bth, cov_x, var_eps, k_cl,
                      seed, n_trials = 1, scov_x = sqrtm(cov_x)) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
    xs <- scov_x %*% matrix(rnorm(2 * k_cl), 2, k_cl)
    ys <- t(bt) %*% xs + 
      sqrt(var_eps) * matrix(rnorm(2 * k_cl), 2, k_cl)
    yhats <- t(bth) %*% xs
    te_cl <- knn(t(yhats), t(ys), 1:k_cl, k = 1)
    mr <- sum(te_cl != 1:k_cl)/k_cl
    mrs[i] <- mr
  }
  mean(mrs)
}


simulate <- function(bt, bth, cov_x, var_eps, k_cl,
                      seeds, n_trials = 1,
                     scov_x = sqrtm(cov_x), mc.cores = 1) {
  tempf <- function(seed) 
    simulate0(bt, bth, cov_x, var_eps, k_cl, seed, n_trials, scov_x)
  unlist(mclapply(seeds, tempf, mc.cores = mc.cores))
}

simulate0(bt, bth, cov_x, var_eps, k_cl, 1, n_trials, scov_x)

simulate(bt, bth, cov_x, var_eps, k_cl, 1:50, n_trials, mc.cores = 25)
