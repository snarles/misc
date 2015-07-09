library(pracma)
library(MASS)
source("stats253/covariance.R")
n <- 500
n_tr <- 400
xs <- 2 * randn(n, 2)
D <- distmat(xs, xs)
theta0 <- c(2, 3)
ck <- exp2.cov.class(theta0)
Sigma <- ck(D)

beta <- rnorm(2)
y <- as.numeric(xs %*% beta + mvrnorm(1, mu = zeros(n, 1), Sigma = Sigma))
x_tr <- xs[1:n_tr, ]
y_tr <- y[1:n_tr]
x_te <- xs[-(1:n_tr), ]
y_te <- y[-(1:n_tr)]
D_tr <- D[1:n_tr, 1:n_tr]

res <- lm(y_tr ~ x_tr + 0)
ck_h <- estimate.cov.fun(res$residuals, D = D_tr, cov.class = exp2.cov.class, plot = TRUE)
lines(1:50/5, ck(1:50/5), col = "blue", lwd = 2)

Sigma_h <- ck_h(D_tr)
Sigma_ha <- ck_h(D)

beta
res$coefficients
yh_ols <- x_te %*% res$coefficients
btg <- gls(y = y_tr, X = x_tr, Sigma = Sigma_h)
pre <- gls(y = y_tr, X = x_tr, Sigma = Sigma_h,
           SigmaX0_X = Sigma_ha[-(1:n_tr), 1:n_tr], X0 = x_te)
plot(y_te, pre)
points(y_te, yh_ols, col = "red")

