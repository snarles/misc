library(pracma)
n <- 100
p <- 20
X <- randn(n, p) #%*% randn(p)
x0 <- rnorm(p)

h1 <- as.numeric(t(x0) %*% solve(t(X) %*% X, t(X)))
z <- as.numeric(X %*% x0)
h2 <- sum(x0^2) * z/sum(z^2)
plot(h1, h2)
abline(0, 1)
list(c(sum(abs(h1)), sum(abs(h2))),
     c(sum(h1^2), sum(h2^2)))