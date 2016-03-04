####
##  Variable selection for faster bootstrap
####

library(pracma)
library(glmnet)
library(MASS)

f2 <- function(x, y=0) sum((x - y)^2)

true_pred_err_dist <- function(n, bt, nte, mc.reps = 100, 
                               lchoice = "lambda.min", ...) {
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    p <- length(bt); ntr <- n - nte
    X <- randn(n, p)
    y <- X %*% bt + rnorm(n)
    res <- cv.glmnet(X[1:ntr, ], y[1:ntr], ...)
    yh <- predict(res, X[-(1:ntr), ], s=res[[lchoice]])
    err <- f2(y[-(1:ntr)], yh)
    errs[i] <- err
  }
  errs
}

elnet_boot_err_dist <- function(X, y, nte, mc.reps = 100, 
                                lchoice = "lambda.min", ...) {
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    inds.tr <- sample(nrow(X), n - nte, replace = FALSE)
    res <- cv.glmnet(X[inds.tr, ], y[inds.tr], ...)
    yh <- predict(res, X[-inds.tr, ], s=res[[lchoice]])
    err <- f2(y[-(inds.tr)], yh)
    errs[i] <- err
  }
  errs
}

mc.reps <- 1e3

n <- 100
p <- 200; k <- 20
bt <- c(rep(1, k), rep(0, p - k)) * 2
nte <- 20; ntr <- n - nte

## elnet settings
alpha = 0.5

##errs <- true_pred_err_dist(n, bt, nte)

X <- randn(n, p)
y <- X %*% bt + rnorm(n)
res <- cv.glmnet(X[1:ntr, ], y[1:ntr], alpha = alpha)
yh <- predict(res, X[-(1:ntr), ], s=res$lambda.min)
(err <- f2(y[-(1:ntr)], yh))

t1 <- proc.time()
elnet.boot.errs <- elnet_boot_err_dist(X, y, nte, alpha = alpha, mc.reps = mc.reps)
(full.time <- proc.time() - t1)
mean(elnet.boot.errs)

## try to do variable selection
res2 <- glmnet(X, y, alpha = alpha)
names(res2)
cts <- apply(res2$beta, 2, function(v) sum(v != 0))
ind <- which(cts > 20)[1]
bt <- coef(res2, s = res2$lambda[ind])
vars <- which(bt[-1] != 0)

t1 <- proc.time()
reduced.boot.errs <- elnet_boot_err_dist(X[, vars], y, nte, alpha = alpha, mc.reps = mc.reps)
(red.time <- proc.time() - t1)


layout(matrix(1:2, 2, 1))
hist(elnet.boot.errs)
hist(reduced.boot.errs)

c(full.time[3], red.time[3])
