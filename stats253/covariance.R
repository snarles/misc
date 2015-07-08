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
  
  # DONE 2: estimate the covariance function at lags h
  # this should depend on the range of ds
  Nres <- 20
  h <- seq(from=min(ds), to=max(ds), length.out=Nres)
  sample.cov <- sapply(1:(Nres - 1), function(i) {mean(es[ds >= h[i] & ds < h[i + 1]])})
  sample.se <- sapply(1:(Nres - 1), function(i) {sd(es[ds >= h[i] & ds < h[i + 1]])})
  counts <- sapply(1:(Nres - 1), function(i) {length(es[ds >= h[i] & ds < h[i + 1]])})
  
  
  # DONE 3: Choose parameters theta to minimize the (weighted) sum of squares between 
  #         sample.cov and cov.class(theta). This can be done by a grid search or 
  #         using "optim" in R.
  hmid <- (h[-1] + h[-length(h)]) / 2
  theta2obj <- function(theta = 1/hmid[1]^2, vals = FALSE) {
    cov.est <- cov.class(c(1, theta))
    fcov <- cov.est(hmid)
    res <- lm(sample.cov ~ fcov + 0, weights = counts)
    if (vals) {
      return(c(res$coefficients, theta))
    }
    sum(res$residuals^2 * counts)
  }
  bestt <- optimise(theta2obj, interval = c(1/max(hmid)^2, 1/min(hmid)^2))
  theta.est <- theta2obj(bestt$minimum, TRUE)
  cov.est <- cov.class(theta.est)
  # plot the sample covariances, 95% confidence band, and estimated covariance function
  if(plot) {
    # draw sample covariances
    plot(hmid, sample.cov, cex = Nres/2 * sqrt(counts/sum(counts)))
    
    # DONE 4: calculate the "standard errors"
    std.errors <- sample.se/sqrt(counts - 1)
    upper.lim <- sample.cov + 1.96 * std.errors
    lower.lim <- sample.cov - 1.96 * std.errors
    
    # draw confidence bands
    polygon(c(hmid, rev(hmid)), c(upper.lim, rev(lower.lim)), col=rgb(0,0,0,.3))
    
    # plot estimated covariance function
    t <- seq(0, max(h), length.out=1000)
    lines(t, cov.est(t))
  }
  
  return(cov.est)
}

# DONE 5: compute the GLS estimator
# If X0=NULL, then it returns the coefficients; otherwise, it 
# returns the predictions for the observations in X0
gls <- function(y, X, Sigma, SigmaX0_X = NULL, X0=NULL) {
  gram <- t(X) %*% solve(Sigma, X)
  xty <- t(X) %*% solve(Sigma, y)
  coefs <- solve(gram, xty)
  cov <- solve(gram)
  ses <- sqrt(diag(cov))
  print(cbind(coef=coefs, se=ses))
  resids <- y - X %*% coefs
  if(is.null(X0)) {
    return(coefs)
  } else {
    preds <- X0 %*% coefs + SigmaX0_X %*% solve(Sigma, resids)
    return(preds)
  }
}