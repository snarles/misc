####
##  Using parametric bootstrap to shortcut elastic net bootstrap
####

library(pracma)
library(glmnet)
library(MASS)

f2 <- function(x, y=0) sum((x - y)^2)

true_pred_err_dist <- function(n, bt, nte, mc.reps = 100) {
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    p <- length(bt); ntr <- n - nte
    X <- randn(n, p)
    y <- X %*% bt + rnorm(n)
    res <- cv.glmnet(X[1:ntr, ], y[1:ntr], intercept = FALSE)
    yh <- predict(res, X[-(1:ntr), ], s=res$lambda.min)
    err <- f2(y[-(1:ntr)], yh)
    errs[i] <- err
  }
  errs
}

elnet_boot_err_dist <- function(X, y, nte, mc.reps = 100) {
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    inds.tr <- sample(nrow(X), n - nte, replace = FALSE)
    res <- cv.glmnet(X[inds.tr, ], y[inds.tr], intercept = FALSE)
    yh <- predict(res, X[-inds.tr, ], s=res$lambda.min)
    err <- f2(y[-(inds.tr)], yh)
    errs[i] <- err
  }
  errs
}

boot_err_dist <- function(X, y, nte, mc.reps = 100) {
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    inds.tr <- sample(nrow(X), n - nte, replace = FALSE)
    yh <- X[-inds.tr, ] %*% ginv(X[inds.tr, ]) %*% y[inds.tr]
    err <- f2(y[-(inds.tr)], yh)
    errs[i] <- err
  }
  errs
}

parametric_boot_err_dist <- function(X, y, nte, mc.reps = 100) {
  yh <- X %*% ginv(X) %*% y; y0 <- y
  errs <- numeric(mc.reps)
  for (i in 1:mc.reps) {
    y <- yh + rnorm(length(y))
    inds.tr <- sample(nrow(X), n - nte, replace = FALSE)
    ya <- X[-inds.tr, ] %*% ginv(X[inds.tr, ]) %*% y[inds.tr]
    err <- f2(y[-(inds.tr)], ya)
    errs[i] <- err
  }
  errs
}


n <- 100
p <- 200; k <- 20
bt <- c(rep(1, k), rep(0, p - k)) * 2
nte <- 10; ntr <- n - nte

errs <- true_pred_err_dist(n, bt, nte)

X <- randn(n, p)
y <- X %*% bt + rnorm(n)
res <- cv.glmnet(X[1:ntr, ], y[1:ntr], intercept = FALSE)
yh <- predict(res, X[-(1:ntr), ], s=res$lambda.min)
(err <- f2(y[-(1:ntr)], yh))

inds <- which(coef(res, s = res$lambda.min)[-1] != 0)
elnet.boot.errs <- elnet_boot_err_dist(X[, inds], y, nte)
mean(elnet.boot.errs)


boot.errs <- boot_err_dist(X[, inds], y, nte)
mean(boot.errs)

pboot.errs <- parametric_boot_err_dist(X[, inds], y, nte)
mean(pboot.errs)


layout(matrix(1:4, 2, 2))
hist(errs)
hist(elnet.boot.errs)
hist(boot.errs)
hist(pboot.errs)

err
