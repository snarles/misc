orthocomplete <- function(a, b) {
  d2 <- dim(a)[2]
  ab <- cbind(a, b)
  res <- qr(ab)
  b2 <- qr.Q(res)[, -(1:d2)]
  signs <- sign(apply(b2 * b, 2, sum))
  b2 <- b2 %*% diag(signs)
  cbind(a, b2)
}

align_sign <- function(a1, a2) {
  temp <- t(a1) %*% a2
  inds <- apply(temp, 1, function(v) { order(-abs(v))[1]  })
  signs <- sign(temp[cbind(1:dim(a1)[2], inds)])
  ans <- matrix(0, dim(a2)[2], dim(a2)[2])
  ans[cbind(1:dim(a1)[2], inds)] <- signs
  t(ans)
}


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

p <- 5
r <- 2
apa <- matrix(rnorm(r * p), r, p)
temp <- svd(apa)
apa <- temp$u %*% diag(1:r) %*% t(temp$v)
gma <- rnorm(r)

n <- 80000
l <- 40000
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

v_a <- orthocomplete(res_a$v, res_x$v[, -(1:r)])
permmat <- align_sign(res_x$v, v_a)
v_a <- v_a %*% permmat
d_a <- t(permmat) %*% diag(c(res_a$d, numeric(p - r))) %*% permmat
eta <- cbind(res_a$u, matrix(0, r, p - r)) %*% permmat
#apa2 <- eta %*% d_a %*% t(v_a)
#sum(abs(apa - apa2))

#image(t(v_a) %*% res_x$v)
delta_v <- t(v_a) %*% res_x$v - diag(rep(1, p))
#max(abs(diag(delta_v)))
#image(delta_v)
delta_z <- (t(z_l) %*% z_l/l) - diag(rep(1, r))
#image(delta_z)

yh_ridge3 <- (1/n) * z %*% eta %*% d_a %*% t(v_a) %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  v_a %*% d_a %*% t(eta) %*% 
  t(z_l) %*% z_l %*% gma

#plot(yh_ridge3 + err_term1 + err_term2, yh_ridge2)

error_term3 <- (1/n) * z %*% eta %*% d_a %*% delta_v %*%
  diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  v_a %*% d_a %*% t(eta) %*% 
  t(z_l) %*% z_l %*% gma

error_term4 <- (1/n) * z %*% eta %*% d_a %*% t(v_a) %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(delta_v) %*%
  d_a %*% t(eta) %*% 
  t(z_l) %*% z_l %*% gma

error_term5 <- z %*% eta %*% d_a %*% t(v_a) %*%
  (res_x$v) %*% diag(1/(res_x$d^2 + lada)) %*% t(res_x$v) %*%
  v_a %*% d_a %*% t(eta) %*% 
  delta_z %*% gma

hist(yh_ridge[-(1:l)])
hist(err_term1[-(1:l)])
hist(err_term2[-(1:l)])

hist(yh_ridge3[-(1:l)])
hist(error_term3[-(1:l)])
hist(error_term4[-(1:l)])
hist(error_term5[-(1:l)])
hist((error_term3 + error_term4 + error_term5)[-(1:l)])

hist(delta_v)
hist(delta_z)
1/(res_x$d^2 + lada)
