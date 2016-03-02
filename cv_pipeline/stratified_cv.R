#####
##  Stratified cross-validation
#####

library(pracma)
library(MASS)
library(class)

block_design_counts <- function(n, p) {
  ncs <- floor(n * p)
  while (sum(ncs) < n) {
    tf <- p - ncs/n; tf <- pmax(0, tf)
    alloc <- rmultinom(1, size = 1, prob = tf/sum(tf))
    (ncs <- ncs + alloc[, 1])
  }
  ncs
}

invtable <- function(ncs) {
  n <- sum(ncs); k <- length(ncs)
  temp <- cumsum(c(0,ncs))[1:k] + 1
  z <- numeric(n)
  z[temp] <- 1
  z <- cumsum(z)
  nonzero <- (1:k)[ncs > 0]
  z <- nonzero[z]
  z
}

## generates data from Gaussian mixture
gen_data <- function(n, p, mus, Sigmas = NULL, block.design = FALSE) {
  if (length(p) == 1) {
    p <- c(p, 1-p)
  }
  k <- length(p)
  if (is.null(dim(mus))) mus <- t(mus)
  d <- dim(mus)[2]
  if (is.null(Sigmas)) {
    Sigmas <- list()
    for (i in 1:length(p)) Sigmas[[i]] <- eye(d)
  }
  ns <- NA * p
  ## determine class counts in sample
  if (block.design) {
    ncs <- block_design_counts(n, p)
  } else {
    ncs <- rmultinom(1, size = n, prob = p)[, 1]
  }
  ## make class assignment vector
  z <- invtable(ncs)
  ## sample gaussians
  ans <- matrix(0, n, d)
  for (cl in unique(z)) {
    ans[z == cl, ] <- mvrnorm(sum(z == cl), mus[cl, ], Sigmas[[cl]])
  }
  list(cl = z, Y = ans)
}

cv_knn <- function(Y, cl, nte, stratify = FALSE, mc.reps = 100, seed = NULL, ...) {
  cl <- as.factor(cl)
  if (!is.null(seed)) set.seed(seed)
  ps <- numeric(mc.reps)
  tab <- table(cl)
  for (ii in 1:mc.reps) {
    n <- length(cl)
    if (stratify) {
      inds_te <- c()
      cts <- block_design_counts(n = nte, p = table(cl)/n)
      for (i in levels(cl)) {
        inds_te <- c(inds_te, sample(which(cl == i), cts[i], replace = FALSE))
      }
    } else {
      inds_te <- sample(1:n, nte)
    }
    clh <- knn(train = Y[-inds_te, ], cl = cl[-inds_te],
               test = Y[inds_te, , drop = FALSE])
    ps[ii] <- sum(clh != cl[inds_te])/nte
  }
  ps
}


lineId::zattach(gen_data(10, 0.55, eye(3)[1:2, ], block.design = TRUE))
mc.reps <- 1000
min(table(cl))/length(cl)
mean(cv_knn(Y, cl, nte = 3, mc.reps = mc.reps, stratify = TRUE))
mean(cv_knn(Y, cl, nte = 3, mc.reps = mc.reps, stratify = FALSE))

