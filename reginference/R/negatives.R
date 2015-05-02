#' Create artificial, known negative variables
#' 
#' From a matrix X, produces random projections of X with added gaussian noise
#' @param x The (n x p) matrix or data frame X
#' @param sigma The noise level relative to the norm of the new columns
#' @param q The number of columns to be generated this way
#' @param name The column name
#' @param adjoin Whether or not to adjoin the result z to x
#' @return An (n x q) matrix or data frame of random projections.  The class of the output matches the class of the input.
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

