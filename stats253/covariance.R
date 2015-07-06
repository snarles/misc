#####################################
### TOOLS FOR COVARIANCE MODELING ###
#####################################

# exp.cov.class() takes in a set of parameters and returns 
# the exponential covariance function with those parameters.
# For example, f <- exp.cov.class(c(1,1)) is a function, 
# which we can then evaluate, e.g., f(0) = 0.
exp.cov.class <- function(theta) {
  return(function(h) theta[1]*exp(-theta[2]*h))
}

# DONE 1: Make a similar function to exp.cov.class but for a 
# different class of covariance functions (e.g., triangular, 
# Gaussian, Matern).

exp2.cov.class <- function(theta) {
  return(function(h) theta[1]*exp(-theta[2]*h^2))
}


# estimate.cov.fun takes in:
#   - e: a vector of length n, 
#   - D: an n x n matrix of distances between observations
#   - cov.class: a class of covariance functions, such as exp.cov.class
#   - theta.init: initial guess of parameters in covariance function
#   - plot: whether or not to plot the covariance function
# and returns the covariance function.
estimate.cov.fun <- function(e, D, cov.class, theta.init, plot=TRUE) {
  
  # calculate matrix that gives e_i*e_j for all i, j
  E <- e %*% t(e)
  
  # extract upper triangular to avoid double-counting pairs
  es <- E[upper.tri(E)]
  ds <- D[upper.tri(D)]
  
  # TODO 2: estimate the covariance function at lags h
  # this should depend on the range of ds
  Nres <- 20
  h <- seq(from=min(ds), to=max(ds), length.out=Nres)
  sample.cov <- sapply(1:(Nres - 1), function(i) {mean(es[ds >= h[i] * ds < h[i + 1]])})
  counts <- sapply(1:(Nres - 1), function(i) {length(es[ds >= h[i] * ds < h[i + 1]])})
  
  
  # TODO 3: Choose parameters theta to minimize the (weighted) sum of squares between 
  #         sample.cov and cov.class(theta). This can be done by a grid search or 
  #         using "optim" in R.
  theta.est <- theta.init
  
  
  cov.est <- cov.class(theta.est)
  
  # plot the sample covariances, 95% confidence band, and estimated covariance function
  if(plot) {
    # draw sample covariances
    plot(h, sample.cov)
    
    # TODO 4: calculate the "standard errors"
    std.errors <- c()
    upper.lim <- sample.cov + 1.96 * std.errors
    lower.lim <- sample.cov - 1.96 * std.errors
    
    # draw confidence bands
    polygon(c(h, rev(h)), c(upper.lim, rev(lower.lim)), col=rgb(0,0,0,.3))
    
    # plot estimated covariance function
    t <- seq(0, max(h), length.out=1000)
    lines(t, cov.est(t))
  }
  
  return(cov.est)
}

# TODO 5: compute the GLS estimator
# If X0=NULL, then it returns the coefficients; otherwise, it 
# returns the predictions for the observations in X0
gls <- function(y, X, Sigma, X0=NULL) {
  coefs <- c()
  ses <- c()
  print(cbind(coef=coefs, se=ses))
  
  if(is.null(X0)) {
    return(coefs)
  } else {
    preds <- c()
    return(preds)
  }
}