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

#'@title simulation_params
#'@param sigma_mu matrix : covariance of mu
#'@param sigma_eps scalar: noise level of epsilon
#'@param k : number of classes to randomly draw
simulation_params <- function(
  sigma_mu, sigma_eps, k)
{
  obj <- list(sigma_mu = sigma_mu,
              sigma_eps = sigma_eps,
              k = k)
  structure(obj, class = "simulation_params")
}

run_simulation <- function()