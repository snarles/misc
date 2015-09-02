####
##  Fun with random matrices and ridge regression
####

library(pracma)
library(MASS)
library(magrittr)
library(parallel)
f2 <- function(x, y = 0) sum((x- y)^2)

## Computes inverse of v(z)
## ws, ts are discrete distribution H(t)
## gm is gamma
zfunc <- function(vs, gm, ws, ts) {
  lv <- length(vs)
  lt <- length(ts)
  vv <- repmat(vs, lt, 1)
  tt <- repmat(t(t(ts)), 1, lv)
  wt <- repmat(t(t(ws * ts)), 1, lv)
  temp <- wt/(1 + tt * vv)
  -1/vs + gm * colSums(temp)
}



vs <- seq(1/10, 10, 1/10)
ts <- c(1, 0.5)
ws <- c(.5, .5)
gm <- 0.5


## compute zs
zs <- zfunc(vs, gm, ws, ts)
## compute m
ms <- (vs + (1/zs))/gm - (1/zs)
ms <- vs/gm + ((1/gm) - 1)/zs
## check z
lv <- length(vs)
lt <- length(ts)
tt <- repmat(t(t(ts)), 1, lv)
ww <- repmat(t(t(ws)), 1, lv)
vv <- repmat(vs, lt, 1)
zz <- repmat(zs, lt, 1)
iv <- zs - gm * colSums(tt * ww/(1 + tt * vv))
f2(-1/vs, iv)
## check m
mm <- repmat(ms, lt, 1)
m2 <- colSums(ww/(tt * (1 - gm - gm * zz * mm) - zz))
f2(ms, m2)
#f2(gm * (ms + 1/zs), vs + 1/zs)


####
##  Confirm RMT results
####

gamma <- 2
n <- 1000
p <- gamma * n
Sigma <- (1/2/p) * randn(p, 2 * p) %>% { .  %*% t(.) }
ws <- rep(1/p, p)
ts <- eigen(Sigma)$values

## compute theoretical
vs <- seq(1/10, 2, 1/10)
zs <- zfunc(vs, gm, ws, ts)
ms <- vs/gm + ((1/gm) - 1)/zs
lps <- -1/gm * (1/(zs * vs) + 1)
is <- 1 + zs * ms

## compute empirical
xtr <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
SigmaH <- (1/n) * xtr %>% { t(.) %*% .}
m_emp <- function(z) 1/p * sum(diag(solve(SigmaH - z * eye(p))))
lp_emp <- function(z) 1/p * sum(diag(solve(SigmaH - z * eye(p), Sigma)))
i_emp <- function(z) 1/p * sum(diag(solve(SigmaH - z * eye(p), SigmaH)))
ms_e <- unlist(mclapply(zs, m_emp, mc.cores = 3))
lp_e <- unlist(mclapply(zs, m_emp, mc.cores = 3))
is_e <- unlist(mclapply(zs, i_emp, mc.cores = 3))
is_e2 <- 1 + zs * ms_e

View(cbind(zs, ms, ms_e))
View(cbind(zs, lps, lp_e))
View(cbind(zs, is, is_e, is_e2))


