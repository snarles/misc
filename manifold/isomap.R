library(rgl)
library(vegan)

ffun <- function(u) {
  n <- dim(u)[1]
  x <- matrix(0, n, 4)
  x[, 1] <- u[, 1] + u[, 2]
  x[, 2] <- u[, 1] - u[, 2]
  x[, 3] <- 2 * u[, 1] - u[, 2]
  x[, 4] <- u[, 1] + 2 * u[, 2]
  return (cbind(x, x))
}

noisify <- function(x, sigma = 1) {
  x + sigma * rnorm(length(x))
}


n <- 2000
p <- 2
u <- matrix(2* runif(n * p) - 1, n, p)
u_cols <- rgb((u[, 1] + 1) * .4, (u[, 2] + 1) * .4, 0)
sig_x <- ffun(u)
x <- noisify(sig_x, 0.1)



help(isomap)

res <- isomap(dist(x), k = 10, ndim = 2)
y <- scores(res)
dim(y)
plot(y[, 1], y[, 2], col = u_cols, pch = '.', cex = 10)
