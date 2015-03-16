##############################################################
#' Initialize the gaussian_dist class
#'
#' @seealso \code{\link{initialize}}
#'
#' @keywords internal
#'
#' @rdname initialize-methods
setMethod(
  "initialize", "gaussian_dist",
  function(.Object, covariance, ...) {
    sqrt_c <- sqrtm(covariance)
    isqrt_c <- isqrtm(covariance)
    dimension <- as.integer(dim(covariance)[1])
    callNextMethod(.Object, covariance = covariance,
                  sqrt_c = sqrt_c, isqrt_c = isqrt_c,
                  dimension = dimension, ...)
})
##############################################################
#' Create an instance of the gaussian dist
#'
#' @param covariance The covariance matrix
#' @export
new_gaussian <- function(covariance){
  new("helloworld", covariance)
}
##############################################################