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

## does not do feature selection (returns all variables)
extract_all <- function(X, opts = NULL) diag(rep(1, dim(X)[2]))

## selects k variables (columns) with highest marginal variances
extract_high_variance <- function(X, opts = NULL) {
  X <- as.matrix(X)
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
  X <- as.matrix(X)
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
fit_ols <- function(data, opts = NULL) {
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(intercept = TRUE)
  if (opts$intercept) {
    res <- lm(y ~ X, data = data)
  } else {
    res <- lm(y ~ X + 0, data = data)
  }
  res
}


####
##  Prediction and cross-validation procedures
##------------------------------------------
##  Each procedure takes a training dataset and possibly a test dataset,
##  a specification of a linear feature extraction subroutine and a model fitting subroutine
##  and produces some kind of average MSE generalization error estimate.
##   * test_error:
##       Estimates the generalization error of the method using training and test data.
##   * full_cv_error:
##       Estimates generalization error using cross-validation,
##       performing feature extraction separately within each fold.
##   * partial_cv_error:
##       Estimates generalization error using CV, but using features from the full training data!
##  INPUTS: Data (y, X) should be in a matrix, with the response y as the first column
##  ARGUMENTS: 
##             (1) `extractor`: Feature extraction subroutine,
##             (2) `extract_opts`: options for feature extraction subroutine
##             (3) `fitter`: Model fitting subroutine
##             (4) `fit_opts`: options for moddel fitting subroutine
##             (5) `train_data`: training data,
##             and additional arghuments depending on procedure
####

test_error <- function(extractor=extract_all,
                       extract_opts = NULL, 
                       fitter = fit_ols, 
                       fit_opts = NULL, 
                       train_data, test_data) {
  train_data <- as.matrix(train_data)
  test_data <- as.matrix(test_data)
  y_tr <- train_data[, 1]
  X_tr <- train_data[, -1]
  ## extract features
  dirs <- extractor(X_tr, extract_opts)
  Z <- X_tr %*% dirs
  trdat <- data.frame(y = y, X = I(Z))
  ## fit model
  res <- fitter(trdat, opts = fit_opts)
  ## predict  
  y_te <- test_data[, 1]
  X_te <- test_data[, -1]
  Z_te <- X_te %*% dirs
  tedat <- data.frame(y = y_te, X = I(Z_te))
  yhat <- predict(res, tedat)
  ## mse
  mean((yhat - y_te)^2)
}




