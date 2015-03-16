#' Sample Points
#' 
#' @export
#' @docType methods
#' @rdname sample_points-methods
#' @param omega The distribution from which to sample
#' @param n The number of observations to sample
setGeneric("sample_points", function(omega, n){
  standardGeneric("sample_points")
})
#' @export
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