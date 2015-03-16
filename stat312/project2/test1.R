## testing source.R

## volume formulae

source("source.R")
d <- 3L
cb <- new("cube", 10, d)
volume(cb)

b1 <- new("ball", 2, d)
volume(b1)
4/3 * pi * 2^3

N_MC <- 1e5
x <- sample_points(cb, N_MC)
sum(in_space(b1, x))/N_MC
volume(b1)/volume(cb)

e1 <- new("ellipsoid", diag(c(1, 2, 3)))
dimension(e1)
e1@ax
y <- sample_points(e1, 100)
apply(y, 1, max)
in_space(e1, y)
volume(e1)

sum(in_space(e1, x))/N_MC
volume(e1)/volume(cb)

cm <- cov(matrix(rnorm(d^2), d, d))
z <- t(mvrnorm(N_MC, rep(0, d), cm))
e_l <- quantile_ellipsoid(cm, 0.1)
e_u <- quantile_ellipsoid(cm, 0.9)
sum(in_space(e_l, z))/N_MC
sum(in_space(e_u, z))/N_MC

