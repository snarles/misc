n <- 2000
p <- 100

x <- matrix(rnorm(n * p), n, p)
y <- rnorm(n)
eye <- diag(rep(1, p))
lda <- 1.5
res <- svd(x)
res$d

y_ridge <- x %*% solve(t(x) %*% x + lda * eye) %*% t(x) %*% y
y_ridge2 <- res$u %*% diag(res$d^2/(res$d^2 + lda) )%*% t(res$u) %*% y

y_ridge - y_ridge2
