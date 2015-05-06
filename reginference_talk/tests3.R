
alphas <- 1:100/100

x <- x_gal
y <- y_gal

cov_test_results <_ function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  res <- lars(as.matrix(x), y)
  res2 <- covTest(res, as.matrix(x), y)
  res3 <- res2$results
  res4 <- res3[complete.cases(res3), ]
  pvs <- res4[, 3]
  vars <- abs(res4[, 1])
  nrejs <- numeric(100)
  nsncs <- numeric(100)
  dim(res3)
  length(unique(res3[, 1]))
  rej <- c()
  cur_alph <- 1
  for (i in 1:length(pvs)) {
    if (pvs[i] > cur_alph/100) {
      next_alph <- ceiling(pvs[i] * 100)
      nrejs[cur_alph:next_alph] <- length(unique(vars[1:i]))
      nsncs[cur_alph:next_alph] <- sum(negs[unique(vars[1:i])])
      cur_alph <- next_alph
    }
  }
  ngood <- nrejs - nsncs
  cbind(1:100/100, nsncs, ngood)  
}

sslasso_results <- function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  res <- SSLasso(as.matrix(x), y, verbose = FALSE)
  names(res)
  sd <- (res$up.lim - res$unb.coef)/qnorm(.975)
  for (i in 1:100) {
    
  }
}

knockoff_results <- function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  nrejs <- numeric(100)
  nsncs <- numeric(100)
  res <- knockoff.filter(x, y)
  W <- res$statistic
  for (i in 1:100) {
    t <- knockoff.threshold(W, i/100, "knockoff")
    selected <- which(W >= t)
    nrejs[i] <- length(selected)
    nsncs[i] <- sum(negs[selected])
  }
  cbind(1:100/100, nsncs, ngood)  
}



dim(res2$results)
