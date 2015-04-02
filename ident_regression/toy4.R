p <- 5
q <- 5

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
bt <- 1e-1 * matrix(1:(q * p), q, p)
als <- lapply(1:1e3, function(i) bt + 1e-4 * matrix(rnorm(q * p), q, p))
#als <- c(list(bt), als)
n_trials <- 10
k_cl <- 30
res <- matrix(0, length(als), 30)
for (i in 1:length(als)) {
    al <- als[[i]]
    res[i, ] <- unlist(mclapply(gsd + 1:30, simulate, mc.cores = 30))
}
proc.time()

xmat <- matrix(0, length(als), p * q)
for (i in 1:length(als)) xmat[i, ] <- als[[i]]
y <- apply(res, 1, mean)
reg <- lm(y ~ xmat)
grad <- coef(reg)[-1]
grad <- grad/sqrt(sum(grad^2))

gsd2 <- 324e3
n_trials <- 1e3
al <- bt
c1 <- unlist(mclapply(gsd2 + 1:30, simulate, mc.cores = 30))
al <- bt - 1e-3 * grad
c2 <- unlist(mclapply(gsd2 + 1:30, simulate, mc.cores = 30))
mean(c1 - c2)
sd(c1 - c2)/sqrt(30)




