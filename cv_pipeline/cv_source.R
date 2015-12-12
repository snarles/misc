#####################################
##  Cross-validating ML pipelines  ##
#####################################

## See 'cv_source_test'.R for unit tests
## Written by Charles Zheng, 2015
## URL: https://github.com/misc/cv_pipeline

library(rARPACK)

####
## Feature extraction subroutines
####

## selects k variables (columns) with highest marginal variances
extract_high_variance <- function(X, opts = NULL) {
  p <- dim(X)[2]
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(k = 10)
  k <- opts$k
  if (k > p) stop("k cannot exceed dim(X)[2]")
  ## compute marginal variances
  mus <- colMeans(X)
  X.c <- t(t(X - mus))
  sds <- sqrt(colMeans(X.c^2))
  ## return variables with k highest variances
  inds <- order(-sds)[1:k]
  ans <- matrix(0, p, k)
  ans[cbind(inds, 1:k)] <- 1
  ans
}

## returns k principal directions (PCA), plus standardization constants
extract_pcs <- function(X, opts = NULL) {
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(k = 10, standardize = TRUE)
  k <- opts$k
  if (k > p) stop("k cannot exceed dim(X)[2]")
  ## scale and center the matrix, recording the scaling params
  if (opts$standardize) {
    mus <- colMeans(X)
    X.c <- t(t(X - mus))
    sds <- sqrt(colMeans(X.c^2))
    X.s <- t(t(X/sds))      
  }
  else {
    sds <- rep(1, p)
    X.s <- X
  }
  ## compute SVD
  res <- svds(X.s, k)
  v <- res$v
  ## scale v to cancel out the standardization
  ans <- v / sds
  ans
}

####
##  Model fitting subroutines and prediction methods
####

## fits OLS and returns the fit object
fit_ols <- function(X, y, opts = NULL) {
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(intercept = TRUE)
  if (opts$intercept) {
    res <- lm(y ~ X)
  } else {
    res <- lm(y ~ X + 0)    
  }
  res
}


####
##  Prediction and cross-validation pipelines
##------------------------------------------
##  Each 'pipeline' takes a training dataset and possibly a test dataset,
##  a specification of a linear feature extraction subroutine and a model fitting subroutine
##  and produces some kind of average MSE generalization error estimate.
##   * generalization_error_pipeline:
##       Estimates the generalization error of the method using training and test data.
##   * full_cv_pipeline
##       Estimates generalization error using cross-validation,
##       performing feature extraction separately within each fold.
##   * partial_cv_pipeline
##       Estimates generalization error using CV, but using features from the full training data!
####


