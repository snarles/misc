source('metric/wishart_formulae.R')

get_cov <- function(n, p) {
  x <- matrix(rnorm(n * p), n, p)
  w <- solve((1/n) * t(x) %*% x)
  x <- matrix(rnorm(n * p), n, p)
  norms_x <- apply(x, 1, function(x) sum(x ^ 2))
  lambda <- eigen(w)$values
  z <- x %*% diag(sqrt(lambda))
  norms_z <- apply(z, 1, function(x) sum(x ^ 2))
  gam <- p/n
  res1 <- c(cov(norms_x, norms_z), 2 * n * p/(n - p - 1), 2 * n * gam/(1 - gam))
  res2 <- c(var(norms_x), 2 * p, 2 * gam * n)  
  res3 <- c(var(norms_z),
            2 * n^2/((n-p)*(n-p-1)*(n-p-3)) * ((n-p-2) * p + p^2 + p),
            2 * gam * (n + 1)/(1 - gam)^3)#,
            #2 * n^2 * eTrWi2(n, eye(p)))
  res4 <- c(cor(norms_x, norms_z), res1[2]/sqrt(res2[2] * res3[2]),
            sqrt(1-gam))
  list(res1, res2, res3, res4)
}


get_cov(200, 100)

