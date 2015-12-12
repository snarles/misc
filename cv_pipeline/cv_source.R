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
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(k = 10)
  k <- opts$k
  ## compute marginal variances
  mus <- colMeans(X)
  X.c <- t(t(X - mus))
  sds <- sqrt(colMeans(X.c^2))
  ## return variables with k highest variances
  ans <- list(inds = order(-sds)[1:k])
  ans
}

## returns k principal directions (PCA), plus standardization constants
extract_pcs <- function(X, opts = NULL) {
  ## implement default arguments this way for compatibility
  if (is.null(opts)) opts <- list(k = 10)
  k <- opts$k
  ## scale and center the matrix, recording the scaling params
  mus <- colMeans(X)
  X.c <- t(t(X - mus))
  sds <- sqrt(colMeans(X.c^2))
  X.s <- t(t(X/sds))  
  ## compute SVD
  res <- svds(X.s, k)
  dirs <- res$v
  ans <- list(mus = mus, sds = sds, dirs = dirs)
  ans
}

