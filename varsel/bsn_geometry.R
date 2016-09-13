### Exploring geometry of Best Subset Null problem
## Q_i are n x k matrices with orthogonal columns
## What is the space {y: min_i ||Q_i y||^2 > t ||y||^2} look like?

## Conjecture: 2 mirror copies of a simply connected piece

library(pracma)

## WARNING!! use global variable Qs
tau_func0 <- function(y) {
  vs <- sapply(Qs, function(Q) sum((t(Q) %*% y)^2))
  min(vs)/sum(y^2)
}

Qprods <- function(Qs, y) {
  vs <- sapply(Qs, function(Q) sum((t(Q) %*% y)^2))
  vs/sum(y^2)
}
normalize <- function(x) {
  x/sqrt(sum(x^2))
}


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

## refine an optimum further
alternating_proj <- function(Qs, y, nits = 100, eps.0 = 0.1) {
  eps <- eps.0
  for (it.no in 1:nits) {
    qsp <- Qprods(Qs, y)
    ind.min <- which.min(qsp)
    yp <- normalize(Qs[[ind.min]] %*% t(Qs[[ind.min]]) %*% y)
    newpts <- sapply(1:10, function(i) {
      min(Qprods(Qs, normalize(y + eps * i * yp)))
    })
    if (max(newpts) < min(qsp)) {
      eps <- eps/2
    } else {
      y <- normalize(y + eps * which.max(newpts) * yp)
      #print(Qprods(Qs, y))
    }
  }
  y
}

####
##  TESTS
####

nq <- 5
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



## CCA METHOD
ys_cca <- cca_method(Qs)
head(ys_cca)

## PURE RANDOM METHOD
ys <- randn(n, 1e4)
taus <- apply(ys, 2, tau_func0)
max(taus)

## OPTIMIZATION METHOD
ress <- lapply(1:100, function(i) optim(rnorm(n), tau_func0, 
                                        control = list(fnscale = -1)))
sols <- sapply(ress, `[[`, "par")
sols <- apply(sols, 2, normalize)
vals <- apply(sols, 2, tau_func0)
max(vals)
colnames(sols) <- paste(vals)
sols <- sols[, order(-vals)]
head(sols[, 1:10])
floor(cor(sols[, 1:10]) * 10)

## OPTIMIZATION INITIALIZING WITH CCA
res1 <- optim(ys_cca[, 1], tau_func0, control = list(fnscale = -1))
ress <- lapply(1:100, function(i) optim(ys_cca[, 1:3] %*% runif(3), tau_func0, 
                                        control = list(fnscale = -1)))
sols <- sapply(ress, `[[`, "par")
sols <- apply(sols, 2, normalize)
vals <- apply(sols, 2, tau_func0)
max(vals)
res1$value
colnames(sols) <- paste(vals)
sols <- sols[, order(-vals)]
head(sols[, 1:10])
floor(cor(sols[, 1:10]) * 10)

y_star <- normalize(res1$par)
Qprods(Qs, y_star)
y_star2 <- alternating_proj(Qs, y_star)
Qprods(Qs, y_star2)
