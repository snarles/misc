
alphas <- 1:100/100

x <- x_gal
y <- y_gal

OLS_results <- function(x, y) {
  nrejs <- numeric(100)
  nsncs <- numeric(100)
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  res <- lm(y ~ as.matrix(x))
  res2 <- summary(res)$coefficients[-1, ]
  cf <- res2[, 1]
  sd <- res2[, 2]
  pv <- res2[, 4]
  for (i in 1:100) {
    rej <- which(pv < i/100)
    nrejs[i] <- length(rej)
    nsncs[i] <- sum(negs[rej])
  }
  ngood <- nrejs - nsncs
  cbind(1:100/100, nsncs, ngood)  
}

cov_test_results <- function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  res <- lars(as.matrix(x), y)
  res2 <- covTest(res, as.matrix(x), y, maxp = 50)
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
  nsncs[100] <- sum(negs)
  nrejs[100] <- min(dim(x))
  ngood <- nrejs - nsncs
  cbind(1:100/100, nsncs, ngood)  
}

sslasso_results <- function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  res <- SSLasso(as.matrix(x), y, verbose = FALSE)
  nrejs <- numeric(100)
  nsncs <- numeric(100)
  names(res)
  sd <- (res$up.lim - res$unb.coef)/qnorm(.975)
  for (i in 1:100) {
    rej <- which(abs(res$unb.coef) > sd * qnorm(1 - (i/200)))
    nrejs[i] <- length(rej)
    nsncs[i] <- sum(negs[rej])
  }
  ngood <- nrejs - nsncs
  cbind(1:100/100, nsncs, ngood)  
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
  ngood <- nrejs - nsncs
  cbind(1:100/100, nsncs, ngood)  
}

res_mar <- function(x, y) {
  negs <- sapply(names(x), function(v) substr(v, 0, 3) == "Neg")
  cors <- cor(x, y)
  rej <- order(-abs(cors))
  sncs <- negs[rej]
  nsncs <- cumsum(sncs)
  ngood <- 1:dim(x)[2] - nsncs
  cbind(nsncs, ngood)
}


res_lars <- function(obj) {
  res2 <- unique(names(unlist(obj$actions)))
  negs <- sapply(res2, function(v) substr(v, 0, 3) == "Neg")
  nsncs <- cumsum(negs)  
  cbind(nsncs, 1:length(res2) - nsncs)
}

## plot codes

