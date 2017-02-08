library(pracma)
n <- 4
p <- 2
X <- randn(n, p) %*% randn(p)
x0 <- rnorm(p)

h1 <- as.numeric(t(x0) %*% solve(t(X) %*% X, t(X)))
z <- as.numeric(X %*% x0)
h2 <- sum(x0^2) * z/sum(z^2)
plot(h1, h2)
abline(0, 1)
