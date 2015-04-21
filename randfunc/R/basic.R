#' Creates a random linear function
#' 
#' This function returns a closure with given input dimensionality and coefficients
#' @param ndim Dimension of input (x)
#' @param var Marginal variance of coefficients, default = 1
#' @param sparsity Sparsity of coefficients, defaul = ndim
#' @param positive Constrain the coefficients to positive? default = False
#' @keywords linear
#' @export
#' @examples
#' f <- random_linear(10, 1, 2)
#' f(1:10)
#' f()

random_linear <- function(ndim, var = 1, sparsity = ndim, positive = FALSE)
{
  intercept <- sqrt(var) * rnorm(1)
  bt <- numeric(ndim)
  bt[sample(ndim, sparsity, FALSE)] <- sqrt(var) * rnorm(sparsity)
  if (positive) bt <- abs(bt)
  ans <- function(x) {
    if (is.null(dim(x))) x <- t(x)
    as.numeric(x %*% bt) + intercept
  }
  ans
}

