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
    mus[, i] <- apply(mat[, groups==ug[i]], 1, mean)
    covs[, , i] <- cov(t(mat[, groups==ug[i]]))
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

do_gauss_class <- function(resp, index, classes, ntraining) {
  resp <- resp[, index %in% classes]
  index <- index[index %in% classes]
  ug <- unique(index)
  training_filt <- create_training_inds(index, ntraining)
  temp <- mu_covs_by_group(resp[, training_filt],
                           index[training_filt])
  mu_est <- temp$mus
  cov_est <- apply(temp$covs, c(1, 2), mean)
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
              training_inds = training_inds)
  return(ans)
}
