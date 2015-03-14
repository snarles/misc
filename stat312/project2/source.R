library(MASS)


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
    k = "numeric", d = "numeric",
    rej_prob = "numeric"
  )
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
    k <- dim(centers)[2]    
    stopifnot(k == dim(covariances)[3])
    stopifnot(k == length(weights))
    d <- dim(centers)[1]
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
        x <- x[, in_space(omega@domain, x)]
        if (dim(x)[2] > tup[2]) flag <- FALSE
      }
      ans[, lbls == tup[1]] <- x[, 1:tup[2]]
    }
    ans
  }
)


