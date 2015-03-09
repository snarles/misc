get_filter <- function(beta_vec, wav.pyr, intercept = FALSE) {
  if (intercept) beta_vec <- beta_vec[-1]
  inds <- which(beta_vec != 0)
  cplx <- cos(2*pi*runif(length(inds))) + sin(2*pi*runif(length(inds)))
  filter <- t(cplx * t(wav.pyr[, inds])) %*% beta_vec[inds]
  filter <- matrix(filter, nrow=128, byrow=FALSE)
  return (Re(filter))
}