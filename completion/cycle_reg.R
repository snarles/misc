####
##  CYCLIC REGRESSION
####

library(magrittr, quietly=TRUE, warn.conflicts=FALSE)
library(pracma, quietly=TRUE, warn.conflicts=FALSE)
library(Matrix, quietly=TRUE, warn.conflicts=FALSE, verbose=FALSE)
library(glmnet, quietly=TRUE, warn.conflicts=FALSE, verbose=FALSE)
#library(softImpute, quietly=TRUE, warn.conflicts=FALSE)


## Cyclic OLS

cylic_ols <- function(mat, n_its = 10) {
  pos <- is.na(mat)
  p <- dim(mat)[2]
  mu <- apply(mat, 2, function(v) mean(v, na.rm = TRUE))
  mat[pos] <- mu[col(mat)[pos]]
  for (i in 1:n_its) {
    for (i in 1:p) {
      inds_tr <- which(!pos[, i])
      X_tr <- mat[inds_tr, -i]
      y_tr <- mat[inds_tr, i]
      X_te <- mat[-inds_tr, -i]
      cf <- lm(y_tr ~ X_tr)$coefficients
      y_hat <- cf[1] + X_te %*% cf[-1]
      mat[-inds_tr, i] <- y_hat
    }    
  }
  mat
}

## NN completion with column term

f2 <- function(x) sum(x^2)

st <- function(z, l) sign(z) * pmax(abs(z) - l, 0)

nn_objective_f <- function(Z, N, cc, lambda, ...) {
  n <- dim(Z)[1]
  M <- N + ones(n, 1) %*% cc
  f2(Z[!is.na(Z)] - M[!is.na(Z)]) + lambda * sum(svd(N,0,0)$d)
}

nn_init_params <- function(Z, lambda = 0.1, rho = 0.1) {
  n <- dim(Z)[1]; p <- dim(Z)[2]
  z <- zeros(n, p)
  list(Z = Z, M = z, N = z, cc = zeros(1, p), W = z,
       lambda = lambda, rho = rho)  
}

nn_iterate <- function(Z, M, N, cc, W, lambda = 0.1, rho = 1) {
  n <- dim(Z)[1]; p <- dim(Z)[2]
  mp <- (!is.na(Z)) + 0
  Zo <- Z
  Zo[is.na(Z)] <- 0
  ## M iterate
  temp <- N + ones(n, 1) %*% cc + W
  M <- ((mp * Zo) + (rho/2) * temp)/(mp + (rho/2))
  ## N iterate
  temp <- M - ones(n, 1) %*% cc - W
  res <- svd(temp)
  N <- res$u %*% (st(res$d, lambda/rho) *  t(res$v))
  ## c iterate
  temp <- -N + M - W
  cc <- t(colMeans(temp))
  ## W iterate
  W <- W + (N - M + ones(n, 1) %*% cc)
  list(Z = Z, M = M, N = N, cc = cc, W = W, lambda = lambda, rho = rho)
}

nn_optim <- function(Z, n_its = 10, lambda = 0.1, rho = 1) {
  pars <- nn_init_params(Z, lambda, rho)
  ofs <- numeric(n_its)
  for (i in 1:n_its) {
    pars <- do.call(nn_iterate, pars)
    ofs[i] <- do.call(nn_objective_f, pars)
  }
  M <- with(pars, N + ones(dim(N)[1], 1) %*% cc)
  Z[is.na(Z)] <- M[is.na(Z)]
  c(pars, list(ofs = ofs, fitted = M, imputed = Z))
}

mean_impute <- function(Z) {
  mu <- apply(Z, 2, function(v) mean(v, na.rm = TRUE))
  Z[is.na(Z)] <- mu[col(Z)[is.na(Z)]]
  Z
}

####
##  Load Data
####

tab <- readRDS('data/16pf.rds')
tab <- tab[tab$age < 100 & !is.na(tab$age), ]
(n <- dim(tab)[1])
n_tr <- 1000
mat0 <- as.matrix(tab[sample(n, n_tr), 1:163])
p_m <- 0.05
na.inds <- matrix(rbinom(length(mat0), 1, p = p_m), nrow = n_tr) == 1
mat <- mat0; mat[na.inds] <- NA

t1 <- proc.time()
res_nn <- nn_optim(mat, n_its = 10, lambda = 10, rho = 1)
proc.time() - t1

t1 <- proc.time()
mat_c <- cylic_ols(mat, n_its = 10)
proc.time() - t1


f2(mat0 - mean_impute(mat))
f2(mat0 - res_nn$imputed)
f2(mat0 - mat_c)


