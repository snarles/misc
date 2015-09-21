## Reference: Multivariate Statistics, by Fujikoshi Ulyanov and Shimizu

library(pracma)
library(MASS)

eWiAWi_formula <- function(n, Sigma, A = eye(dim(Sigma)[1]), empirical = 0) {
  p <- dim(Sigma)[1]
  if (empirical == 0) {
    c2 <- 1/((n - p) * (n - p - 1) * (n - p - 3))
    c1 <- (n - p - 2) * c2
    ans <- c1 * solve(Sigma, A) %*% solve(Sigma) + 
      c2 * (solve(Sigma, t(A)) %*% solve(Sigma) +
              sum(diag(solve(Sigma, t(A)))) * solve(Sigma))
    return(ans)
  } else {
    res <- array(0, dim = c(p, p, empirical))
    xall <- mvrnorm(n=n * empirical, mu=rep(0, p), Sigma = Sigma)
    for (i in 1:empirical) {
      xx <- xall[((i-1) * n) + (1:n), ]
      WW <- t(xx) %*% xx
      res[, , i] <- solve(WW, A) %*% solve(WW)
    }
    res2 <- apply(res, c(1, 2), mean)
    return(res2)
  }
}

eTrWi2 <- function(n, Sigma) {
  p <- dim(Sigma)[1]
  c2 <- 1/((n - p) * (n - p - 1) * (n - p - 3))
  c1 <- (n - p - 2) * c2
  lambdas <- eigen(Sigma)$values
  ans <- c1 * sum(1/lambdas^2) + 
    c2 * (sum(1/lambdas^2) +sum(1/lambdas)^2)  
  ans
}


# n <- 10
# Sigma <- eye(5)
# A <- randn(5)
# res1 <- eWiAWi_formula(n, Sigma, A)
# res2 <- eWiAWi_formula(n, Sigma, A, empirical = 1e5)
# res1
# res2

# n <- 12
# sum(diag(eWiAWi_formula(n, Sigma)))
# eTrWi2(n, Sigma)

