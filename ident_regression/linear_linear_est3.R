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


# simulate multiple bths

simulate0 <- function(mu_bt, bthz, cov_x, var_eps, k_cl,
                      seed, n_trials = 1, scov_x = sqrtm(cov_x),
                      var_bt = 0) {
  set.seed(seed)
  n_bthz <- length(bthz)
  mrs <- matrix(0, n_trials, n_bthz)
  for (i in 1:n_trials) {
    bt <- mu_bt + sqrt(var_bt) * matrix(rnorm(p^2), p, p)
    xs <- scov_x %*% matrix(rnorm(p * k_cl), p, k_cl)
    ys <- t(bt) %*% xs + 
      sqrt(var_eps) * matrix(rnorm(p * k_cl), p, k_cl)
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


p <- 7
mu_bt <- diag(rep(0, p))
var_eps <- 1
cov_x <- diag(rep(1, p))
seed <- 1
scov_x <- sqrtm(cov_x)
k_cl <- 10
n_trials <- 1e4
mc.cores <- 25

var_bt <- 9
bthzA <- list(); bthzB <- list(); bthzC <- list()
scales <- seq(0, 10, 0.2)
#for (i in 1:length(scales)) bthzA[[i]] <- matrix(scales[i] * c(1, 1, 1, 1), 2, 2)
#for (i in 1:length(scales)) bthzB[[i]] <- matrix(scales[i] * c(1, 1, 1, -1), 2, 2)
for (i in 1:length(scales)) bthzC[[i]] <- scales[i] * diag(rep(1, p))

bthzC

#simulate0(mu_bt, bthz, cov_x, var_eps, k_cl, 1, n_trials, scov_x, var_bt)
proc.time()
#resA <- simulate(mu_bt, bthzA, cov_x, var_eps, k_cl, 1:25, n_trials, mc.cores = 25)
#resB <- simulate(mu_bt, bthzB, cov_x, var_eps, k_cl, 1:25, n_trials, mc.cores = 25)
resC <- simulate(mu_bt, bthzC, cov_x, var_eps, k_cl, 1:25, n_trials, mc.cores = 25, var_bt = var_bt)
proc.time()
#plot(apply(resA, 2, mean))
#plot(apply(resB, 2, mean))
plot(scales, apply(resC, 2, mean))

cbind(scales, apply(resC, 2, mean))

