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

# simulate multiple bths

simulate0 <- function(mu_bt, bthz, cov_x, var_eps, k_cl,
                      seed, n_trials = 1, scov_x = sqrtm(cov_x),
                      var_bt = 0) {
  set.seed(seed)
  n_bthz <- length(bthz)
  mrs <- matrix(0, n_trials, n_bthz)
  for (i in 1:n_trials) {
    bt <- mu_bt + sqrt(var_bt) * matrix(rnorm(4), 2, 2)
    xs <- scov_x %*% matrix(rnorm(2 * k_cl), 2, k_cl)
    ys <- t(bt) %*% xs + 
      sqrt(var_eps) * matrix(rnorm(2 * k_cl), 2, k_cl)
    for (j in 1:n_bthz) {
      bth <- bthz[[j]]
      yhats <- t(bth) %*% xs
      te_cl <- knn(t(yhats), t(ys), 1:k_cl, k = 1)
      mr <- sum(te_cl != 1:k_cl)/k_cl
      mrs[i, j] <- mr
    }
  }
  apply(mrs, 2, mean)
}

simulate <- function(mu_bt, bthz, cov_x, var_eps, k_cl,
                      seeds, n_trials = 1,
                     scov_x = sqrtm(cov_x), var_bt = 0, mc.cores = 1) {
  #simulate0 <- function(mu_bt, bthz, cov_x, var_eps, k_cl,
  #                      seed, n_trials = 1, scov_x = sqrtm(cov_x),
  #                      var_bt = 0) {  
  tempf <- function(seed) 
    simulate0(mu_bt, bthz, cov_x, var_eps, k_cl, seed,
              n_trials, scov_x, var_bt)
  do.call(rbind, mclapply(seeds, tempf, mc.cores = mc.cores))
}



mu_bt <- diag(c(0, 0))
var_eps <- 1
cov_x <- diag(rep(1, 2))
seed <- 1
scov_x <- sqrtm(cov_x)
k_cl <- 4
n_trials <- 10
mc.cores <- 25

var_bt <- 1
bthz <- list()
scales <- c(0.25, 0.5, 1, 2)
for (i in 1:length(scales)) bthz <- c(bthz, list(matrix(scales[i] * c(1, 1, 1, 1), 2, 2)))
for (i in 1:length(scales)) bthz <- c(bthz, list(matrix(scales[i] * c(1, 1, 1, -1), 2, 2)))
for (i in 1:length(scales)) bthz <- c(bthz, list(matrix(scales[i] * c(sqrt(2), 0, 0, sqrt(2)), 2, 2)))

bthz

simulate0(mu_bt, bthz, cov_x, var_eps, k_cl, 1, n_trials, scov_x, var_bt)
res <- simulate(mu_bt, bthz, cov_x, var_eps, k_cl, 1:9, n_trials, mc.cores = 3)
apply(res, 2, mean)
