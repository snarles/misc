#'@importFrom Rcpp evalCpp
#'@useDynLib zheng001

#'@title vball
#'@param d dimension
#'@param r radius
#'@export
vball <- function(d, r = 1) {
  (pi)^(d/2)/gamma((d/2) + 1) * r^d
}

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
    dimension = "integer",
    nc = "numeric"
  ),
  contains = "distribution"
)

#'@title simulation_params-class
#'@rdname simulation_params-class
#'@exportClass simulation_params
setClass(
  "simulation_params",
  representation(
    prior = "distribution",
    sigma = "numeric",
    k = "integer"
  )
)

#'@title simulation_results-class
#'@rdname simulation_results-class
#'@exportClass simulation_results
setClass(
  "simulation_results",
  representation(
    params = "simulation_params",
    external_its = "integer",
    internal_its = "integer",
    id_rates = "numeric",
    id_rate = "numeric"
  )
)


