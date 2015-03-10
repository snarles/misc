get_filter <- function(beta_vec, wav.pyr, intercept = FALSE) {
  if (intercept) beta_vec <- beta_vec[-1]
  inds <- which(beta_vec != 0)
  cplx <- cos(2*pi*runif(length(inds))) + sin(2*pi*runif(length(inds)))
  filter <- t(cplx * t(wav.pyr[, inds])) %*% beta_vec[inds]
  filter <- matrix(filter, nrow=128, byrow=FALSE)
  return (Re(filter))
}

# input: p x n matrix
# output: p x kg matrix, p x p x kg array
mu_covs_by_group <- function(mat, groups) {
  p <- dim(mat)[1]
  ug <- unique(groups)
  kg <- length(ug)
  mus <- matrix(0, p, kg)
  covs <- array(0, c(p, p, kg))
  for (i in 1:kg) {
    mus[, i] <- apply(mat[, groups==ug[i], drop = FALSE], 1, mean)
    covs[, , i] <- cov(t(mat[, groups==ug[i], drop = FALSE]))
  }
  ans <- list(mus = mus, covs = covs)
  return (ans)
}

create_training_inds <- function(index, ntraining) {
  kg <- length(unique(index))
  ans <- rep(FALSE, length(index))
  for (i in unique(index)) {
    ans[sample(which(index == i), ntraining, FALSE)] <- TRUE
  }
  return (ans)
}

isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

do_gauss_class <- function(resp, index, classes, ntraining, ..., 
                           cov_mat) {
  resp <- resp[, index %in% classes]
  index <- index[index %in% classes]
  ug <- unique(index)
  training_filt <- create_training_inds(index, ntraining)
  temp <- mu_covs_by_group(resp[, training_filt],
                           index[training_filt])
  mu_est <- temp$mus
  if (missing(cov_mat)) {
    cov_est <- apply(temp$covs, c(1, 2), mean)
  } else {
    cov_est <- cov_mat
  }
  test_resp <- resp[, !training_filt]
  test_index <- index[!training_filt]
  whiten_mat <- isqrtm(cov_est)
  malas <- fastPdist2(t(whiten_mat %*% test_resp),
                      t(whiten_mat %*% mu_est))
  index_est <- ug[apply(malas, 1, function(v) order(v)[1])]
  err_rate <- sum(test_index != index_est)/length(test_index)
  ans <- list(err_rate = err_rate,
              index_est = index_est,
              test_index = test_index,
              training_filt = training_filt)
  return(ans)
}

library(quadprog)

solve_unif <- function(dm, h) {
  trans_dm <- exp(-(dm/h)^2)
  kg <- dim(dm)[2]
  w <- solve(trans_dm, rep(1, kg))
  if (min(w) > 0) {
    w <- w/sum(w)
  } else {
    res <- solve.QP(trans_dm, rep(0, kg),
                    cbind(rep(1, kg), diag(rep(1, kg))),
                    c(1, rep(0, kg)), meq = 1)
    w <- res$solution
    w[w < 0] <- 0
    w <- w/sum(w)
  }
  return (w)  
}

repeat_function <- function(times, f, ...) {
  results <- list(times)
  for (i in 1:times) {
    results[[i]] <- f(...)
  }
  results
}