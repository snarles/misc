library(MASS)
library(Rcpp)
sourceCpp('pdist.cpp')

# a generic function
# checks if d x N matrix of points are in the space
in_space <- function(omega, x) {
  rep(FALSE, dim(x)[2])
}

# a generic function
# returns d x N matrix of points
sample_points <- function(omega, n = 1) {
  t(rep(NA, n))
}

# function
table_tuples <- function(v) {
  tl <- table(v)
  ul <- as.numeric(names(tl))
  ans <- lapply(1:length(tl), function(i) c(ul[i], tl[i]))
  ans
}

identity_covs <- function(d, k) {
  eye <- diag(rep(1, d))
  ans <- array(rep(eye, k), c(d, d, k))
  ans
}

isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

sqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}


setClass("parameter_space")
setClass("distribution")

setClass(
  "ball", 
  representation(
    radius = "numeric",
    dimension = "integer",
    center = "numeric"),
  contains = c("parameter_space", "distribution")
)

setMethod(
  "initialize", "ball",
  function(.Object, radius, dimension, center, ...) {
    if (missing(center)) center <- rep(0, dimension)
    callNextMethod(.Object, radius = radius,
                   dimension = as.integer(dimension), center = center, ...)
  }
)

# in_space.ball
setMethod(
  "in_space",
  signature(omega = "ball", x = "matrix"),
  function(omega, x) {
    norms <- apply(x - omega@center, 2, function(v) sqrt(sum(v^2)))
    (norms < omega@radius)
  }
)

## sample_points.ball
## Draws uniform distribution from ball
setMethod(
  "sample_points",
  signature(omega = "ball", n = "numeric"),
  function(omega, n) {
    m <- matrix(rnorm(n * omega@dimension), omega@dimension, n)
    m <- apply(m, 2, function(v) v / sqrt(sum(v^2)))
    radii <- runif(n)^(1/omega@dimension) * omega@radius
    m <- t(t(m) * radii)
    m
  }
)

## Class mixture_in_ball
## centers: d x k
## covariances: d x d x k
## weights: k
setClass(
  "mixture_in_ball",
  representation(
    domain = "ball",
    centers  = "matrix",
    covariances = "array",
    weights = "numeric",
    normalizing_constants = "numeric",
    isqrts = "array",
    sqrts = "array",
    k = "integer", d = "integer",
    rej_prob = "numeric"
  ),
  contains = "distribution"
)

setMethod(
  "initialize", "mixture_in_ball",
  function(.Object, domain, centers, covariances, weights, ...)
  {
    # option parameters
    N_MONTE_CARLO <- 1e5
    
    # schema validation and set k, d
    stopifnot(length(dim(covariances)) == 3)
    stopifnot(length(dim(centers)) == 2)
    k <- as.integer(dim(centers)[2])    
    stopifnot(k == dim(covariances)[3])
    stopifnot(k == length(weights))
    d <- as.integer(dim(centers)[1])
    stopifnot(d == domain@dimension)
    stopifnot(d == dim(covariances)[1])
    stopifnot(d == dim(covariances)[2])
    
    # compute isqrts
    isqrts <- array(apply(covariances, 3, isqrtm), c(d, d, k))
    sqrts <- array(apply(covariances, 3, sqrtm), c(d, d, k))    
    
    # compute normalizing constants
    dets <- apply(covariances, 3, det)
    # non-truncated normalizing constant
    nc0 <- (2 * pi)^(-d/2) / dets
    # compute truncation
    rej_prob <- numeric(k)
    for (i in 1:k) {
      smp <- t(mvrnorm(N_MONTE_CARLO,
                      centers[, i], covariances[, , i]))
      rej_prob[i] <- 1 - sum(in_space(domain, smp))/N_MONTE_CARLO
    }
    nc = nc0/(1 - rej_prob)
    callNextMethod(
      .Object, domain = domain, centers = centers,
      covariances = covariances, weights = weights,
      normalizing_constants = nc,
      isqrts = isqrts, sqrts = sqrts,
      k = k, d = d, rej_prob = rej_prob
    )
  }
)

## sample_points.mixture_in_ball
setMethod(
  "sample_points",
  signature(omega = "mixture_in_ball", n = "numeric"),
  function(omega, n) {
    ans <- matrix(0, omega@d, n)
    lbls <- sample(omega@k, n, TRUE,
                  prob = omega@weights)
    tl <- table_tuples(lbls)
    for (tup in tl) {
      flag <- TRUE
      rp <- omega@rej_prob[tup[1]]
      estn <- ceiling((tup[2] + 9)/(1 - rp))
      sq_cov <- omega@sqrts[, , tup[1]]
      while(flag) {
        x <- sq_cov %*% matrix(rnorm(estn * omega@d), omega@d, estn)
        x <- x + omega@centers[, tup[1]]
        x <- x[, in_space(omega@domain, x)]
        if (dim(x)[2] > tup[2]) flag <- FALSE
      }
      ans[, lbls == tup[1]] <- x[, 1:tup[2]]
    }
    ans
  }
)

setClass(
  "simulation_params",
  representation(
    prior = "distribution",
    sigma = "matrix",
    k = "integer",
    n = "integer",
    n_tr = "integer"
  )
)

setClass(
  "simulation_detailed_results",
  representation(
    params = "simulation_params",
    misc_rate = "numeric",
    confusion = "matrix",
    centers = "matrix",
    est_centers = "matrix"
  )
)

setClass(
  "simulation_summary_results",
  representation(
    params = "simulation_params",
    ntimes = "integer",
    misc_rates = "numeric",
    misc_rate = "numeric"
  )
)


simulate_once <- function(pars) {
  NULL
}

confusion_mat <- function(k, true_labels, est_labels) {
  mat <- matrix(table(c(1:k, true_labels), c(1:k, est_labels)), k, k)
  diag(mat) <- diag(mat) - 1
  return (mat)
}

setMethod(
  "initialize", "simulation_detailed_results",
  function(.Object, pars, ...) {
    d <- dim(pars@sigma)[1]
    centers <- sample_points(pars@prior, pars@k)
    mu_noise <- t(mvrnorm(pars@k, rep(0, d), pars@sigma/pars@n_tr))
    est_centers <- centers + mu_noise
    n_te <- pars@n - pars@n_tr
    true_labels <- rep(1:pars@k, each = n_te)
    te_centers <- centers[, true_labels]
    te_noise <- t(mvrnorm(pars@k * n_te, rep(0, d), pars@sigma))
    te_x <- te_centers + te_noise
    wht_mat <- isqrtm(pars@sigma)
    wht_centers <- wht_mat %*% est_centers
    wht_x <- wht_mat %*% te_x
    dm <- fastPdist2(t(wht_centers), t(wht_x))
    est_labels <- apply(dm, 2, function(v) order(v)[1])
    cm <- confusion_mat(pars@k, true_labels, est_labels)
    mr <- sum(est_labels != true_labels)/length(true_labels)
    callNextMethod(.Object,
                   params = pars, misc_rate = mr,
                   confusion = cm, centers = centers,
                   est_centers = est_centers, ...)
  }
)

print.sr <- function(x) {
  cat(paste0("Dimension: ", x@params@prior@d, "\n"))
  cat(paste0("N. classes: ", x@params@k, "\n"))
  cat(paste0("Misc. rate: ", x@misc_rate, "\n"))  
}

setMethod(
  "print",
  signature(x = "simulation_detailed_results"),
  print.sr
)

setMethod(
  "print",
  signature(x = "simulation_summary_results"),
  print.sr
)

setMethod(
  "initialize", "simulation_summary_results",
  function(.Object, pars, ntimes, ...) {
    mrs <- numeric(ntimes)
    for (i in 1:ntimes) {
      sr <- new("simulation_detailed_results", pars)
      mrs[i] <- sr@misc_rate
    }
    mr <- mean(mrs)
    callNextMethod(.Object,
                   params = pars, ntimes = as.integer(ntimes),
                   misc_rates = mrs, misc_rate = mr, ...)
  }
)

density_at <- function(obj, x) {
  return (rep(NA, dim(x)[2]))
}

setMethod(
  "density_at",
  signature(obj = "mixture_in_ball", x = "matrix"),
  function(obj, x) {
    k <- obj@k
    n <- dim(x)[2]
    dm <- matrix(0, k, n)
    for (i in 1:k) {
      wht_x <- obj@isqrts[, , i] %*% (x - obj@centers[, i])
      dm[i, ] <- apply(wht_x, 2, function(v) sum(v^2))
    }
    dens_s <- obj@normalizing_constants * exp(-dm/2)
    ans <- as.vector(t(obj@weights) %*% dens_s)
    ans
  }
)

theoretical_misc_rate <- function(pars, de) {
  NA
}

setMethod(
  "theoretical_misc_rate",
  signature(pars = "simulation_params", de = "numeric"),
  function(pars, de) {
    N_MONTE_CARLO <- 1e5
    prior <- pars@prior
    d <- prior@d
    radius <- pars@prior@domain@radius
    vfrac <- det(pars@sigma) * (sqrt(d) * (1 + 1/pars@n_tr))^d/radius^d
    if (missing(de)) {
      x <- sample_points(prior, N_MONTE_CARLO)
      de <- density_at(prior, x)      
    }
    ans <- 1 - sum(exp(-pars@k * vfrac * de))/length(de)
    ans
  }
)

