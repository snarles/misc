#' Mutivariate least-squares with nuclear norm penalty
#' 
#' Minimizes ||AX - B||^2_F + ridge_penalty * ||X||^2_F subject to nuclear norm(X) < t
#' @param a The n x p matrix A
#' @param b The n x q matrix B
#' @param t The nuclear norm constraint
#' @param x Intitialization
#' @param w Initial weight
#' @param ridge_penalty Ridge penalty
#' @param max_its Maximum iterations
#' @examples
#' a <- matrix(rnorm(20 * 10), 20, 10)
#' res <- svd(matrix(rnorm(10 * 5), 10, 5))
#' x0 <- res$u %*% diag(c(.8, .6, .3, .2, .1)) %*% t(res$v)
#' b <- a %*% x0 + 0.01 * matrix(rnorm(20 * 5), 20, 5)
#' x_hat <- nuclear_least_squares(a, b, 2, max_its = 20)
#' sum((x_hat - x0)^2)
nuclear_least_squares <- function(a, b, t = 1,
                      x = rnorm(dim(a)[2]) %*% t(rnorm(dim(b)[2])),
                      w = 0, ridge_penalty = 0, max_its = 10) {
  x <- t * x/sum(svd(x, 0, 0)$d)
  p <- dim(a)[2]; q <- dim(b)[2]
  ata <- t(a) %*% a; atb <- t(a) %*% b
  it <- 0
  for (it in 1:max_its) {
    it <- it + 1;
    sum((a %*% x - b)^2)
    grad <- 2 * (ata %*% x - atb) + ridge_penalty * x
    res <- rARPACK::svds(-grad, 1)
    dx <- t * res$u %*% t(res$v)
    alpha <- (1 + w)/(it + w)
    x <- (1 - alpha) * x + alpha * dx
  }
  x
}

