library(magrittr)

n <- 2000
l <- 200
p <- 100
r <- 50
r_pcr <- 50
apa <- matrix(rnorm(r * p), r, p)
temp <- svd(apa)
seqq <- (1:r)^2 %>% { sqrt(./sum(.)) } %>% `*`(20)
apa <- temp$u %*% diag(seqq) %*% t(temp$v)
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

res_l <- svd(x_l)
res_n <- svd(x)
tt <- x %*% res_n$v[, 1:r_pcr]
plot(apply(tt, 2, var))
res <- lm(y_l ~ tt[1:l, ])
y_hat_pcr <- res$coefficients[1] + tt %*% res$coefficients[-1]
(sqe_pcr <- sum((y_hat_pcr[-(1:l)] - y[-(1:l)])^2))
tlt <- x %*% res_l$v[, 1:r_pcr]
res <- lm(y_l ~ tlt[1:l, ])
y_hat_pcr0 <- res$coefficients[1] + tlt %*% res$coefficients[-1]
(sqe_pcr0 <- sum((y_hat_pcr0[-(1:l)] - y[-(1:l)])^2))

lada <- 0.01
bt_ridge <- solve(t(x_l) %*% x_l + lada * diag(rep(1, p))) %*% t(x_l) %*% y_l
res_l <- svd(x_l)
res_n <- svd(x)
dim(res_l$v)
dim(res_n$v)
bt_ridge_check <- res_l$v %*% diag(1/(res_l$d^2 + lada)) %*% t(res_l$v) %*% t(x_l) %*% y_l
#plot(bt_ridge, bt_ridge_check)
bt_ridge2 <- res_n$v %*% diag(1/(res_n$d^2 + lada)) %*% t(res_n$v) %*% t(x_l) %*% y_l
#bt_ridge2 <- res_n$v %*% diag(1/(res_n$d^2 + lada)) %*% t(res_n$v) %*% t(x) %*% y
#plot(t(x) %*% y, t(x_l) %*% y_l)
#plot(bt_ridge, bt_ridge2)
yh_ridge <- x %*% bt_ridge
yh_ridge2 <- x %*% bt_ridge2
(sqe_ridge <- sum((yh_ridge[-(1:l)] - y[-(1:l)])^2))
(sqe_ridge2 <- sum((yh_ridge2[-(1:l)] - y[-(1:l)])^2))

tt <- x %*% res_svd$v
tt_l <- tt[1:l, ]
zt_ridge <- solve(t(tt_l) %*% tt_l + lada * diag(rep(1,p))) %*% t(tt_l) %*% y_l
yh_tridge <- tt %*% zt_ridge
#plot(yh_tridge, yh_ridge)
(sqe_tridge <- sum((yh_tridge[-(1:l)] - y[-(1:l)])^2))
zt_ridge2 <- solve(t(tt) %*% tt + lada * diag(rep(1,p))) %*% t(tt_l) %*% y_l
yh_tridge2 <- tt %*% zt_ridge2
(sqe_tridge2 <- sum((yh_tridge2[-(1:l)] - y[-(1:l)])^2))
