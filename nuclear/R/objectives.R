#' Computes the nuclear norm of a matrix
#' 
#' Compute sum(d) of the SVD
#' @param x the matrix
nuclear_norm <- function(x) sum(svd(x, 0, 0)$d)

#' Least-squares objective function
#' 
#' Furnishes closures which evaluate the function and gradient for
#' the transformation of the problem min_X tr((AX - B)'(AX - B))
#' subject to NuclearNorm(X) <= t/2, and optionally solves the problem.
#' @param a the n * p matrix A
#' @param b the n * q matrix B
#' @param t the nuclear norm constrant on X, doubled
#' @param max_its Specify the maximum number of iterations
#' @param iter_mult Specify the iteration multipler
#' @param solve_it Solve the problem now?
#' @return A list with elements \code{d}, the dimension of the new space,
#' \code{f}, the function, \code{gradf}, the gradient,
#' \code{cf}, the curvature constant,
#' \code{convert}, a function which converts the solution of the transformed problem to the original,
#' \code{original_f}, computes the original objective function and nuclear norm
#' If solved, also returns \code{sol}, the solution to the original problem,
#' and \code{value}, the value of the objective function
lsq_obj <- function(a, b, t, max_its = NULL, iter_mult = NULL, solve_it = FALSE) {
  p <- dim(a)[2]
  q <- dim(b)[2]
  d <- p + q
  f <- function(z) {
    zsub <- t * z[1:p, -(1:q)]
    sum(apply(a %*% zsub - b, 2, function(v) sum(v^2)))
  }
  gradf = NULL
  cf = NULL
  convert = NULL
  original_f = NULL
  list(f = f, gradf = gradf, cf = cf,
       convert = convert, original_f = original_f)
}