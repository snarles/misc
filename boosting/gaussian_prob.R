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


res <- lars(xsigs, ysig, normalize = FALSE, intercept = FALSE, type = "lar")
plot(res, xlim = c(0, 0.0001), ylim = c(-100, 100))
aa <- coef(res, 1)
dim(aa)

for (i in rev(1:42))  matplot(museq, t(aa[1:i, , drop = FALSE]), type = "l", col = rainbow(42)[1:i])

xtx <- t(xsigs) %*% xsigs
xty <- t(xsigs) %*% ysig
plot(xty)
xtx0 <- t(xsigs) %*% xsigs[, museq == 0]
plot(museq, xtx0)
for (x in seq(0.56, 0.54, -0.001)) {
  plot(museq, xty - x * xtx0, main = x, type = "l")
}
