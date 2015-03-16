#'@importFrom Rcpp evalCpp
#'@useDynLib zheng001

#'@title isqrtm
#'@param m covariance matrix
#'@export
isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

#'@title sqrtm
#'@param m covariance matrix
#'@export
sqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

#'@title distribution-class
#'@rdname distribution-class
#'@exportClass distribution
setClass("distribution")

#'@title gaussian_dist-class
#'@rdname gaussian_dist-class
#'@exportClass gaussian_dist
setClass(
  "gaussian_dist",
  representation(
    covariance = "matrix",
    sqrt_c = "matrix",
    isqrt_c = "matrix",
    dimension = "integer"
  ),
  contains = "distribution"
)

#' Sample Points
#' 
#' @export
#' @docType methods
#' @rdname sample_points-methods
setGeneric("sample_points", function(omega, n, ...){
  standardGeneric("sample_points")
})
#' @rdname sample_points-methods
#' @aliases sample_points,gaussian_dist,numeric-method
setMethod(
  "sample_points",
  signature(omega = "gaussian_dist", n = "numeric"),
  function(omega, n) {
    d <- omega@dimension
    matrix(rnorm(n * d), n, d) %*% omega@sqrt_c
  }
)