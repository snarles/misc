## fitting a gaussian with other gaussians

library(lars)

nz <- function(v) v[v != 0]

delta <- 0.1
xseq <- seq(-5, 5, delta)
museq <- seq(-2, 2, delta)

sigma2_y <- 2

ysig <- dnorm(xseq, 0, sqrt(sigma2_y))
xsigs <- sapply(museq, function(mu) dnorm(xseq, mu))
plot(xsigs[, 1])


res <- lars(xsigs, ysig)
plot(res)
plot()

aa <- coef(res, 1e-5)
dim(aa)

plot(aa[2, ])

xtx <- t(xsigs) %*% xsigs
xty <- t(xsigs) %*% ysig
plot(xty)
xtx0 <- t(xsigs) %*% xsigs[, museq == 0]
plot(xtx0)
plot(xty - 0.55 * xtx0)
