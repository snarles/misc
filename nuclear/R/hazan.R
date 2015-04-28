#' Hazan's SparseApproxSDP
#' 
#' Uses rARPACK to minimize convex f with given gradient and multiplier (proportional to C_f)
#' @param d The dimension of the space S^(n*n)
#' @param f The function to be minimized, maps S^(n*n) to R
#' @param gradf The gradient of the function, maps S^(n*n) to S^(n*n)
#' @param iter_mult rARPACK will use iter_mult * k max iterations
#' @param max_its Maximum number of iterations 
sparse_approx_sdp <- function(d, f, gradf, iter_mult, max_its, ...) {  
  v0 <- rnorm(d) %>% {./sqrt(sum(.^2))}
  z <- v0 %*% t(v0)
  for (k in 1:max_its) {
    cg <- -gradf(x)
    opts <- list(maxitr = iter_mult * k)
    v_k <- eigs(cg, 1, opts = opts)$vectors
    z <- (k-1)/k * z + (1/k) * (v_k %*% t(v_k))
  }
  z
}
