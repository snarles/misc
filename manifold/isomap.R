library(rgl)
library(vegan)
library(RBGL)
library(graph)
library(Rgraphviz)

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

setup <- function(n, p, sigma) {
  n <<- 2000
  p <<- 2
  u <<- matrix(2* runif(n * p) - 1, n, p)
  u_cols <<- rgb((u[, 1] + 1) * .4, (u[, 2] + 1) * .4, 0)
  signal_x <<- ffun(u)
  x <<- noisify(signal_x, sigma)
}
setup(2000, 2, 0.1)

res <- isomap(dist(x), k = 5, ndim = 2)
y <- scores(res)
dim(y)
plot(y[, 1], y[, 2], col = u_cols, pch = '.', cex = 10)
dm <- as.matrix(dist(x))
dim(dm)

## building a faster isomap

dm2 <- isomapdist(dm, k = 5)
rownames(dm) <- colnames(dm) <- paste0("v", 1:dim(dm)[1])
dmt <- dm
diag(dmt) <- NA
k <- 5
is.na(dmt) <- apply(dmt, 2, function(x) x > x[order(x, na.last = TRUE)[k]])
dmt[is.na(dmt)] <- 0
dmt[dmt == 0] <- t(dmt)[dmt == 0]
#dmt
dmg <- as(dmt, "graphNEL")
plot(dmg)
dmg

dm3 <- floyd.warshall.all.pairs.sp(dmg)
as.matrix(dm2) - t(dm3)
res_mds <- cmdscale(dm3, k = 2, eig = TRUE)
res_mds$points

isomap_fast <- function(dist_, ndim = 10, epsilon = NA, k = NA) {
  dm <- as.matrix(dist_)
  rownames(dm) <- colnames(dm) <- paste0("v", 1:dim(dm)[1])
  diag(dm) <- NA
  if (is.na(epsilon)) {
    is.na(dm) <- apply(dm, 2, function(x) x > x[order(x, na.last = TRUE)[k]])
  } else {
    is.na(dm) <- apply(dm, 2, function(x) x >= epsilon)
  }
  dm[is.na(dm)] <- 0
  dm[dm == 0] <- t(dm)[dm == 0]
  dmg <- as(dm, "graphNEL")
  dm2 <- floyd.warshall.all.pairs.sp(dmg)
  res_mds <- cmdscale(dm2, k = ndim, eig = TRUE)
  res_mds
}

proc.time()
res <- isomap(dist(x), k = 10, ndim = 2)
proc.time()
res2 <- isomap_fast(dist(x), k = 10, ndim = 2)
proc.time()
y <- scores(res)
y <- scores(res2)
plot(y[, 1], y[, 2], col = u_cols, pch = '.', cex = 10)
