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

setClass("parameter_space")
setClass(
  "ball", 
  representation(
    radius = "numeric",
    dimension = "numeric",
    center = "numeric"),
  contains = "parameter_space"
)

setMethod(
  "initialize", "ball",
  function(.Object, radius, dimension, ...) {
    center <- rep(0, dimension)
    callNextMethod(.Object, radius = radius,
                   dimension = dimension, center = center, ...)
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

setClass(
  "gaussian_dist",
  representation(
    domain = "parameter_space"
  )
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
    k = "numeric", d = "numeric"
  )
)

setMethod(
  "initalize", "mixture_in_ball",
  function(.Object, domain, centers, covariances, weights, ...)
  {
    # option parameters
    N_MONTE_CARLO <- 1e5
    
    # schema validation and set k, d
    stopifnot(length(dim(covariances)) == 3)
    stopifnot(length(dim(centers)) == 2)
    k <- dim(centers)[2]    
    stopifnot(k == dim(covariances)[3])
    stopifnot(k == length(weights))
    d <- dim(centers)[1]
    stopifnot(d == domain@dimension)
    stopifnot(d == dim(covariances)[1])
    stopifnot(d == dim(covariances)[2])
    
    
    # compute isqrts
    isqrts <- array(apply(covariances, 3, isqrtm), c(d, d, k))
    
    # compute normalizing constants
    dets <- apply(covariances, 3, det)
    # non-truncated normalizing constant
    nc0 <- (2 * pi)^(-d/2) / dets
    # compute truncation
    
    
    callNextMethod(
      .Object, domain = domain, centers = centers,
      covariances = covariances, weights = weights,
      normalizing_constants = nc,
      isqrts = isqrts, k = k, d = d
    )
  }
)

## sample_points.mixture_in_ball
setMethod(
  "sample_points",
  signature(omega = "mixture_in_ball", n = "numeric"),
  function(omega, n) {
    labels <- sample()
  }
)


d <- 2
b1 <- new("ball", radius = 2, dimension = d)
k <- 3
centers <- sample_points(b1, k)
covs <- identity_covs(d, k)
wts <- (temp <- runif(k))/sum(temp)
mb1 <- new("mixture_in_ball", domain = b1,
           centers = centers, covariances = covs,
           weights = wts)
