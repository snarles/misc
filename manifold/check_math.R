## basic ridge PCR SVD expressions

n <- 2000
p <- 100

x <- matrix(rnorm(n * p), n, p)
y <- rnorm(n)
eye <- diag(rep(1, p))
lda <- 1.5
res <- svd(x)
res$d

k <- 5
tt <- x %*% res$v[, 1:k]

y_ridge <- x %*% solve(t(x) %*% x + lda * eye) %*% t(x) %*% y
y_ridge2 <- res$u %*% diag(res$d^2/(res$d^2 + lda) )%*% t(res$u) %*% y

(y_ridge - y_ridge2)[, 1]

y_pcr <- tt %*% solve(t(tt) %*% tt) %*% t(tt) %*% y
y_pcr2 <- res$u %*% diag(c(rep(1, k), rep(0, p-k)) )%*% t(res$u) %*% y
(y_pcr - y_pcr2)[, 1]

## ridge loss approx

p <- 20
r <- 10
apa <- matrix(rnorm(r * p), r, p)
gma <- rnorm(r)

n <- 40000
l <- 20000
z <- matrix(rnorm(n * r), n, r)
sigma_e <- 1
e_x <- sigma_e * matrix(rnorm(n * p), n, p)
e_y <-  sigma_e * rnorm(n)
x <- z %*% apa + e_x
y <- z %*% gma+ e_y
x_l <- x[1:l, ]
y_l <- y[1:l]
z_l <- z[1:l, ]
e_xl <- e_x[1:l, ]
e_yl <- e_y[1:l]
lada <- 0.01

res_a <- svd(apa)
res_x <- svd(x_l/sqrt(n))

bt_ridge <- solve(t(x_l) %*% x_l + n * lada * diag(rep(1, p))) %*% t(x_l) %*% y_l
yh_ridge <- x %*% bt_ridge

yh_ridge2 <- (1/n) * (z %*% apa + e_x) %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  ( t(apa) %*% t(z_l) %*% z_l %*% gma + t(e_xl) %*% z_l %*% gma +
    t(apa) %*% t(z_l) %*% e_yl + t(e_xl) %*% e_yl)

err_term1 <- (1/n) * (z %*% apa + e_x) %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  ( 000 + t(e_xl) %*% z_l %*% gma +
      t(apa) %*% t(z_l) %*% e_yl + t(e_xl) %*% e_yl)

err_term2 <- (1/n) * e_x %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  t(apa) %*% t(z_l) %*% z_l %*% gma

hist(yh_ridge2)
hist(err_term1)
hist(err_term2)


yh_ridge[1]
(yh_ridge-yh_ridge2)[1]


resid_ridge <- yh_ridge - y
dim(res_a$u)
resid_ridge2 <- z %*% (res_a$u %*% diag(res_a$d^2/(res_x$d[1:r]^2 + lada)) %*% t(res_a$u) %*% gma - gma) - e_y




plot(resid_ridge[-(1:l)], resid_ridge2[-(1:l)])
plot(resid_ridge, -e_y)
