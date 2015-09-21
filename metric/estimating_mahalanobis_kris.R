### Kris' code

get_cor <- function(n, p) {
  x <- matrix(rnorm(n * p), n, p)
  w <- solve(t(x) %*% x)
  norms_x <- apply(x, 1, function(x) sqrt(sum(x ^ 2)))
  lambda <- eigen(w)$values
  z <- x %*% diag(sqrt(lambda))
  norms_z <- apply(z, 1, function(x) sqrt(sum(x ^ 2)))
  cor(norms_x, norms_z)
}

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