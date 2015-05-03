#' Create artificial, known negative variables
#' 
#' From a matrix X, produces random projections of X with added gaussian noise
#' @param x The (n x p) matrix or data frame X
#' @param sigma The noise level relative to the norm of the new columns
#' @param q The number of columns to be generated this way
#' @param name The column name
#' @param adjoin Whether or not to adjoin the result z to x
#' @return An (n x q) matrix or data frame of random projections.  The class of the output matches the class of the input.
#' @export
noised_projections <- function(x, sigma = 0.1, q = dim(x)[2], name = NULL,
                               adjoin = FALSE) {
  if (is.null(name)) {
    if (sigma < Inf) name <- "Neg"
    if (sigma == Inf) name <- "Noise"
  }
  x0 <- scale(as.matrix(x), TRUE, TRUE)
  n <- dim(x0)[1]; p <- dim(x0)[2]
  coeffs <- matrix(rnorm(p * q), p, q)
  if (sigma < Inf) {
    z <- x0 %*% coeffs
    z <- apply(z, 2, function(v) v/sqrt(sum(v^2)))
    z <- z + sigma * matrix(rnorm(n * q), n, q)
  } else {
    z <- sigma * matrix(rnorm(n * q), n, q)
  }
  z <- scale(z, TRUE, TRUE)
  colnames(z) <- paste(name, 1:q, sep = "")
  if (class(x) == "data.frame") z <- data.frame(z)
  if (adjoin) z <- cbind(x, z)
  z
}

#' Nonlinear surface
#' 
#' A synthetic function of the same class as fits produced by local regression
#' @param x The data matrix
#' @param u Control points
#' @param a Value at control points
#' @param w Weights
#' @param h Bandwidth (default 1)
#' @param handle Return a function handle? If FALSE, returns the function values at x
#' @param normalize Adjust the function to have unit variance, zero mean, and zero regression coeffs
#' @return A function handle if \code{handle = TRUE} and a vector if \code{handle = FALSE}
#' @export
nw_surface <- function(x, u, a, w, h = 1, handle = TRUE, normalize = TRUE) {
  x <- as.matrix(x)
  if (!normalize) {
    f <- function(x) {
      if (is.null(dim(x))) x <- t(x)
      x <- as.matrix(x)
      dm <- fastPdist2(x, u)
      dm2 <- exp(-(dm^2)/(2*h))
      dm3 <- t(t(dm2) * w)
      rs <- rowSums(dm3)
      dm4 <- dm3/rs
      as.vector(dm4 %*% a)
    }
    if (handle) {
      return (f)
      
    } else {
      return (f(x))
    }
  }
  f0 <- nw_surface(x, u, a, w, h, TRUE, FALSE)
  y0 <- f0(x)
  n <- dim(x)[1]
  res <- lm(y0 ~ x)
  gm <- res$coefficients[-1]
  yadj <- y0 - res$fitted + res$coefficients[1]
  cc <- mean(yadj)
  nm <- sqrt(sum((yadj - cc)^2)/n)
  f <- function(x) {
    (f0(x) - as.vector(x %*% gm) - cc)/nm
  }
  if (handle) return (f)
  ynew <- (yadj - cc)/nm
  ynew
}

#' Nonlinear surface
#' 
#' A random instance of \code{nw_surface}
#' @param x The data matrix
#' @param k Number of control points
#' @param h Bandwidth (default 1)
#' @param handle Return a function handle? If FALSE, returns the function values at x
#' @param normalize Adjust the function to have unit variance, zero mean, and zero regression coeffs
#' @return A function handle if \code{handle = TRUE} and a vector if \code{handle = FALSE}
#' @export
random_nw_surface <- function(x, k, h = 1, handle = TRUE, normalize = TRUE) {
  x <- as.matrix(x)
  n <- dim(x)[1]
  u <- x[sample(n, k), ]
  w <- rep(1, k)
  a <- rnorm(k)
  nw_surface(x, u, a, w, h, handle, normalize)
}