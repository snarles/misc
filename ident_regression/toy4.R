p <- 2
q <- 2
bt <- matrix(c(4,3,2,3), 2, 2)
library(class)

simulate <- function(al, k_cl, n_trials, seed = 0) {
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
  mrs 
}

del1 <- matrix(c(1, 0, 0, 0), 2, 2)
del2 <- matrix(c(0, 1, 0, 0), 2, 2)
del3 <- matrix(c(0, 0, 1, 0), 2, 2)
del4 <- matrix(c(0, 0, 0, 1), 2, 2)
n_trials <- 1e4
k_cl <- 30
mean(simulate(bt, k_cl, n_trials))
mean(simulate(bt + 0.1 * del1, k_cl, n_trials))
mean(simulate(bt - 0.1 * del1, k_cl, n_trials))
