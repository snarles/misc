source('metric/wishart_formulae.R')

get_cov <- function(n, p) {
  x <- matrix(rnorm(n * p), n, p)
  w <- solve((1/n) * t(x) %*% x)
  x <- matrix(rnorm(n * p), n, p)
  norms_x <- apply(x, 1, function(x) sum(x ^ 2))
  lambda <- eigen(w)$values
  z <- x %*% diag(sqrt(lambda))
  norms_z <- apply(z, 1, function(x) sum(x ^ 2))
  res1 <- c(cov(norms_x, norms_z), 2 * n * p/(n - p - 1))
  res2 <- c(var(norms_x), 2 * p)  
  res3 <- c(var(norms_z),
            2 * n^2/((n-p)*(n-p-1)*(n-p-3)) * ((n-p-2) * p + p^2 + p))#,
            #2 * n^2 * eTrWi2(n, eye(p)))
  res4 <- c(cor(norms_x, norms_z), res1[2]/sqrt(res2[2] * res3[2]))
  list(res1, res2, res3, res4)
}


get_cov(200, 100)

