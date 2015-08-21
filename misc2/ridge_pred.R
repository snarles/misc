####
##  Prediction error of ridge
##  Mostly ported from 
##    https://github.com/dobriban/high-dim-risk-experiments
####

library(pracma)
library(MASS)
library(magrittr)
library(parallel)
f2 <- function(x, y = 0) sum((x- y)^2)
zattach <- function(ll) {
  for (i in 1:length(ll)) {
    assign(names(ll)[i], ll[[i]], envir=globalenv())
  }
}

compute_st <- function(vs, gm, ws, ts) {
  lv <- length(vs)
  lt <- length(ts)
  vv <- repmat(vs, lt, 1)
  tt <- repmat(t(t(ts)), 1, lv)
  wt <- repmat(t(t(ws * ts)), 1, lv)
  ww <- repmat(t(t(ws)), 1, lv)
  temp <- wt/(1 + tt * vv)
  zs <- -1/vs + gm * colSums(temp)
  ms <- vs/gm + ((1/gm) - 1)/zs
  vp <-   1/(1/(vs^2) - gm * colSums(ww * tt^2/(1 + tt * vv)^2))
  list(vs = vs, zs = zs, vp = vp, ms = ms)
}


simulate_ridge_risk_cov <- function(Sigma, n, p, alpha, lambda) {
  X <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  w <- alpha * rnorm(p)/sqrt(p)
  y <- X %*% w + rnorm(n)
  ip <- t(X) %*% y
  what <- solve(t(X) %*% X + n * lambda * eye(p), ip)
  Xte <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  yte <- Xte %*% w + rnorm(n)
  yh <- Xte %*% what
  ytr <- X %*% w + rnorm(n)
  yh_tr <- X %*% what
  c(f2(yte, yh)/n, f2(ytr, yh_tr)/n)
}

n <- 30
grid_size <- 1e3
n_lambda <- 1e2
rate <- 1

gm <- 2
p <- n * gm
g <- linspace(1/(2 * p), 1 - 1/(2 * p), p)
g <- 1/rate * log(1/g)
g <- rep(1, p)
Sigma <- diag(g)
ts <- g #eigen(Sigma)$values
ws <- rep(1/p, p)

lambdas <- linspace(0.1, 2.5, n_lambda)^2

## compute theoretical values for out-of-sample
alpha <- 1
lambda <- gm/(alpha^2)
#vs <- linspace(1/grid_size, 1, grid_size)
vs <- linspace(0, 0.5, 10000)
zattach(compute_st(vs, gm, ws, ts))
ls <- -zs
min((ls - lambda)^2)
vs[order(abs(ls - lambda))[1]]
pred_risk <- (1 + (ls * alpha^2/gm - 1) * (1 - ls * vp/vs))/(ls * vs)
ind <- order(abs(ls - lambda))[1]
pred_out <- pred_risk[ind]

## compute for in-sample
vs <- linspace(0.3, 0.4, 1e4)
zattach(compute_st(vs, gm, ws, 1/ts))
ind <- order(abs(zs + alpha^2/gm))[1]
min(abs(zs + alpha^2/gm))
vs[ind]
pred_in <- 1 + alpha^2 * ms[ind]

## simulation

(sim_risk <- simulate_ridge_risk_cov(Sigma, n, p, alpha, lambda))
sim_risks <- mclapply(1:1000, function(i) simulate_ridge_risk_cov(Sigma, n, p, alpha, lambda),
                      mc.cores = 3)
colMeans(do.call(rbind, sim_risks))
c(pred_out, pred_in)







