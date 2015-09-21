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
  res3 <- c(var(norms_z), 2 * n^2 * eTrWi2(n, eye(p)))
  res4 <- c(cor(norms_x, norms_z), res1[2]/sqrt(res2[2] * res3[2]))
  list(res1, res2, res3, res4)
}


get_cov(20, 10)



n <- 1000 # number of samples
K <- 10 # number of p's to try out
r <- 20 # number of reps per dimension
cors_data <- vector(length = K, mode = "list")
ps <- seq(n * 0.01, n, length.out = K)
for(k in seq_len(K)) {
  print(k)
  cors_data[[k]] <- data.frame(n = n, p = ps[k], cors = replicate(r, get_cor(n, ps[k])))
}
library("data.table")
cors_data <- rbindlist(cors_data)

library("plyr")
library("ggplot2")
ddply(cors_data, .(n, p), summarize, mean = mean(cors), var = var(cors))
ggplot(cors_data) +
  geom_jitter(aes(x = p, y = cors))