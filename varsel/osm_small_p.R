library(pracma)

compute_true_Vs <- function(X, mu, ss) {
  Xmu <- as.numeric(t(X) %*% mu)
  XtX <- t(X) %*% X
  ans <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    as.numeric(t(Xmu[S]) %*% solve(XtX[S, S], Xmu[S]))
  })
  names(ans) <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    paste(S, collapse = ".")
  })
  ans
}

compute_V_diff <- function(X, mu, eps, ss) {
  Xmu <- as.numeric(t(X) %*% mu)
  Xeps <- as.numeric(t(X) %*% eps)
  XtX <- t(X) %*% X
  ans <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    as.numeric(2 * t(Xmu[S]) %*% solve(XtX[S, S], Xeps[S]) + t(Xeps[S]) %*% solve(XtX[S, S], Xeps[S]))
  })
  names(ans) <- apply(ss, 2, function(S) {
    S <- S[S != 0]
    paste(S, collapse = ".")
  })
  ans
}

compute_V_diff_empirical <- function(X, mu, sigma2, ss, n.its = 1e2) {
  draws <- sapply(1:n.its, function(i) {
    max(compute_V_diff(X, mu, sqrt(sigma2) * rnorm(nrow(X)), ss))
  })
  sort(draws)
}

n <- 30
p <- 8
k0 <- 2
k <- 2
sst <- 10
mut <- 0.1
sigma2 <- 1
X <- randn(n, p)
bt0 <- numeric(p)
bt0[sample(p, k0)] <- sst * rnorm(k0)
mu <- mut * rnorm(n) + X %*% bt0
get_Y <- function() mu + sqrt(sigma2) * rnorm(n)
delt <- as.numeric(t(X) %*% mu)

ss <- combn(1:p, k)
Vs_true <- compute_true_Vs(X, mu, ss)
sort(Vs_true, TRUE)[1:10]
(ind_star <- which.max(Vs_true))
#Vs_true

Y <- get_Y()
eps <- Y - mu
Z <- as.numeric(t(X) %*% eps)
Vs_Y <- compute_true_Vs(X, Y, ss)
V_diff <- Vs_Y - Vs_true
V_diff2 <- compute_V_diff(X, mu, eps, ss)
#rbind(V_diff, V_diff2)

V_thres <- compute_V_diff_empirical(X, mu, sigma2, ss, n.its = 1000)
vquants <- quantile(V_thres, 0.1 * 1:10)

V_thres_emp <- compute_V_diff_empirical(X, Y, sigma2, ss, n.its = 1000)
vquants2 <- quantile(V_thres_emp, 0.1 * 1:10)

rbind(vquants, vquants2)

aset_oracle <- which(Vs_Y - min(vquants) > max(Vs_Y) - max(vquants))
list(length(aset_oracle), (ind_star %in% aset_oracle))

aset <- which(Vs_Y - min(vquants2) > max(Vs_Y) - max(vquants2))
list(length(aset), (ind_star %in% aset))
