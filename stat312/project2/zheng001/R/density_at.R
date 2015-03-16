#' Density At
#' 
#' @export
#' @docType methods
#' @rdname density_at-methods
#' @param omega The distribution (p-dimensional)
#' @param x An n x p matrix of points
setGeneric("density_at", function(omega, x){
  standardGeneric("density_at")
})
#' @rdname density_at-methods
#' @aliases density_at,gaussian_dist,matrix-method
setMethod(
  "density_at",
  signature(omega = "gaussian_dist", x = "matrix"),
  function(omega, x) {
    dists <- apply(x %*% omega@isqrt_c,
                   1, function(v) sum(v^2))
    omega@nc * exp(-dists/2)
  }
)