n <- 2000
l <- 100
p <- 100
r <- 10
apa <- matrix(rnorm(r * p), r, p)
gma <- rnorm(r)
z <- matrix(rnorm(n * r), n, r)
sigma_e <- 1
x <- z %*% apa + sigma_e * matrix(rnorm(n * p), n, p)
y <- z %*% gma + sigma_e * rnorm(n)
x_l <- x[1:l, ]
y_l <- y[1:l]
library(glmnet)
res <- cv.glmnet(x_l, y_l, alpha = 0)
y_hat_ridge <- predict(res, x)
(sqe_glmnet <- sum((y_hat_ridge[-(1:l)] - y[-(1:l)])^2))
res_svd <- svd(x)
tt <- x %*% res_svd$v[, 1:10]
res <- lm(y_l ~ tt[1:l, ])
y_hat_pcr <- res$coefficients[1] + tt %*% res$coefficients[-1]
(sqe_pcr <- sum((y_hat_pcr[-(1:l)] - y[-(1:l)])^2))