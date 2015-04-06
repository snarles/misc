## Locally linear embedding

## test function

library(rgl)

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
plot(u[, 1], u[, 2], col = u_cols, pch = '.', cex = 10)

plot3d(sig_x[, 1:3], col = u_cols)
plot3d(sig_x[, c(1, 2, 4)], col = u_cols)

## locally linear embedding, with random neighbors

get_ws <- function(x, k = 10) {
  n <- dim(x)[1]
  dm <- as.matrix(dist(x))
  diag(dm) <- Inf
  ords <- t(apply(dm, 2, order))
  nn_inds <- ords[, 1:k]
  ws <- matrix(0, n, n)
  for (i in 1:n) {
    xi <- x[i, ]
    xs <- t(x[nn_inds[i, ], ])
    xi <- xi - xs[, 1]
    xs <- xs[, -1] - xs[, 1]
    v <- lm(xi ~ xs + 0)$coefficients
    w <- c(1 - sum(v), v)
    ws[i, nn_inds[i, ]] <- w
  }
  ws
}

get_ys <- function(ws, d = 2) {
  n <- dim(ws)[1]
  mm <- -ws
  diag(mm) <- 1 + diag(mm)
  #lmax <- norm(mm, type = "2")^2
  #res <- svd(mm)
  #y <- res$v[, n - (1:d)]
  mmat <- diag(rep(1, n)) - (t(mm) %*% mm)
  res <- eigen(mmat)
  res$values[1:3]
  y <- res$vectors[, 1 + (1:d)]
  y
}

ws <- get_ws(x, 6)
y <- get_ys(ws, 2)
plot(y[, 1], y[, 2], col = u_cols, pch = '.', cex = 10, xlim = c(-.1, .1), ylim = c(-.1, .1))
plot(res$vectors[, 2:3], col = u_cols, pch = '.', cex = 10)

u2 <- scale(u)
t(u2[, 1]) %*% mmat %*% u2[, 1]/sum(u2[, 1]^2)
t(y[, 1]) %*% mmat %*% y[, 1]/sum(y[, 1]^2)
