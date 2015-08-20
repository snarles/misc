library(pracma)
library(parallel)

f2 <- function(x) sum(x^2)

errs <- function(n, p) {
  xtr <- randn(n, p)
  xte <- randn(n, p)
  
  bt <- rnorm(p)
  ytr <- xtr %*% bt + rnorm(n)
  ytr2 <- xtr %*% bt + rnorm(n)
  bth <- lm(ytr ~ xtr + 0)$coefficients
  yte <- xte %*% bt + rnorm(n)
  ytr_h <- xtr %*% bth
  yte_h <- xte %*% bth
  
  err_tr <- f2(ytr2 - ytr_h)/n
  err_te <- f2(yte - yte_h)/n
  c(err_tr, err_te)
}

n <- 30
p <- 10
res <- mclapply(1:10000, function(i) errs(n, p), mc.cores = 3)
res <- do.call(rbind, res)

colMeans(res)
c(1 + (p/n), 1 + (p/(n - p - 1)))





