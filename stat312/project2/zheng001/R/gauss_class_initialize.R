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
    nc <- (2 * pi * det(covariance))^(-dimension/2)
    callNextMethod(.Object, covariance = covariance,
                  sqrt_c = sqrt_c, isqrt_c = isqrt_c,
                  dimension = dimension, nc = nc, ...)
})
##############################################################
#' Create an instance of the gaussian dist
#'
#' @param covariance The covariance matrix
#' @export
#' @examples
#' cm <- diag(rep(1, 3))
#' gd <- new_gaussian(cm)
#' x <- sample_points(gd, 10)
#' de <- density_at(gd, x)
new_gaussian <- function(covariance){
  new("gaussian_dist", covariance)
}
##############################################################


#' Create an instance of the simulation params
#'
#' @param prior The distribution
#' @param sigma The noise level of observed points (square root of sigma^2)
#' @param k The number of classes
#' @export
new_simulation_pars <- function(prior, sigma, k){
  new("simulation_params", 
      prior = prior, sigma = sigma, k = as.integer(k))
}

#' Run a simulation
#' 
#' @param params The simulation meta-parameters
#' @param external_its
#' The number of outer loops where parameters are randomized
#' @param internal_its
#' The number of inner loops where random variables are drawn
#' @details
#' \code{id_rates}
#' A vector of identification rates per outer loop
#' \code{id_rate}
#' The average across all trials
#' @export
run_simulation <- function(params, external_its, internal_its) {
  d <- params@prior@dimension
  id_rates <- numeric(external_its)
  for (i in 1:external_its) {
    centers <- sample_points(params@prior, params@k)
    true_labels <- sample(params@k, internal_its, TRUE)
    noise <- params@sigma *
               matrix(rnorm(internal_its * d), internal_its, d)
    points <- centers[true_labels, , drop = FALSE] + noise
    dm <- fastPdist2(centers, points)
    est_labels <- apply(dm, 2, function(v) order(v)[1])
    id_rates[i] <- sum(est_labels == true_labels)/length(true_labels)
  }
  id_rate <- mean(id_rates)
  new(
    "simulation_results",
    params = params, external_its = as.integer(external_its),
    internal_its = as.integer(internal_its), id_rates = id_rates,
    id_rate = id_rate
  )
}