### Exploring geometry of Best Subset Null problem
## Q_i are n x k matrices with orthogonal columns
## What is the space {y: min_i ||Q_i y||^2 > t ||y||^2} look like?

## Conjecture: 2 mirror copies of a simply connected piece

library(pracma)

nq <- 3
n <- 30
p <- 50
X <- randn(n, p)
k <- 5
Qs <- lapply(1:nq, function(i) {
  #S <- randn(n, k)
  S <- X[, sample(p, k, replace = FALSE)]
  Q <- qr.Q(qr(S))
  Q
})

cca_method <- function(Qs, nv = 5) {
  mat <- zeros(k * nq)
  for (i in 1:nq) {
    for (j in 1:nq) {
      if (i == j) {
        mat[(1:k) + (i-1) * k, (1:k) + (j-1) * k] <- eye(k)
      } else {
        mat[(1:k) + (i-1) * k, (1:k) + (j-1) * k] <- t(Qs[[i]]) %*% Qs[[j]]        
      }
    }
  }
  res <- eigen(mat)
  ys <- do.call(cbind, Qs) %*% res$vectors[, 1:nv]
  ys <- apply(ys, 2, normalize)
  colnames(ys) <- paste(apply(ys, 2, tau_func0))
  ys
}

tau_func0 <- function(y) {
  vs <- sapply(Qs, function(Q) sum((t(Q) %*% y)^2))
  min(vs)/sum(y^2)
}

tau_func <- function(y) {
  vs <- sapply(Qs, function(Q) sum((t(Q) %*% y)^2))
  -min(vs) + sum(y^2)
}

tau_func(rnorm(n))

normalize <- function(x) {
  x/sqrt(sum(x^2))
}

## CCA METHOD
ys_cca <- cca_method(Qs)
head(ys_cca)

## PURE RANDOM METHOD
ys <- randn(n, 1e4)
taus <- apply(ys, 2, tau_func0)
max(taus)

## OPTIMIZATION METHOD
ress <- lapply(1:100, function(i) optim(rnorm(n), tau_func))
sols <- sapply(ress, `[[`, "par")
sols <- apply(sols, 2, normalize)
vals <- apply(sols, 2, tau_func0)
max(vals)
colnames(sols) <- paste(vals)
sols <- sols[, order(-vals)]
head(sols[, 1:10])
#cor(t(sols[, 1:10]))
