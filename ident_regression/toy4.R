p <- 10
q <- 10

library(class)
library(parallel)

simulate <- function(seed) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
    xs <- matrix(rnorm(p * k_cl), p, k_cl)
    ys <- t(bt %*% xs + matrix(rnorm(q * k_cl), q, k_cl))
    yhats <- t(al %*% xs)
    te_cl <- knn(t(t(yhats)), t(t(ys)), 1:k_cl, k = 1)
    mr <- sum(te_cl != 1:k_cl)/k_cl
    mrs[i] <- mr
  }
  mean(mrs) 
}

gsd <- 123e3
set.seed(gsd)
proc.time()
bt <- 5e-2 * matrix(1:100, 10, 10)
als <- lapply(1:3, function(i) bt + 1e-4 * matrix(rnorm(100), 10, 10))
als <- c(list(bt), als)
n_trials <- 2e5
k_cl <- 30
res <- matrix(0, length(als), 30)
for (i in 1:length(als)) {
    al <- als[[i]]
    res[i, ] <- unlist(mclapply(gsd + 1:30, simulate, mc.cores = 30))
}
proc.time()

gsd2 <- 324e3

mus <- apply(res, 1, mean)

mus[1] - min(mus)
apply(res, 1, sd)/sqrt(30)
mus[1] - mus

al <- bt
c1 <- unlist(mclapply(gsd2 + 1:30, simulate, mc.cores = 30))
al <- als[[order(mus)[1]]]
c2 <- unlist(mclapply(gsd2 + 1:30, simulate, mc.cores = 30))
mean(c1 - c2)
sd(c1 - c2)/sqrt(30)




