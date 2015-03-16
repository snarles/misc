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

cm <- cov(matrix(rnorm(d^2), d, d)) + diag(rep(1, d))
z <- t(mvrnorm(N_MC, rep(0, d), cm))
nz <- isqrtm(cm) %*% z
nms <- apply(nz, 2, function(v) sum(v^2))
sum(nms < qchisq(0.1, d))/N_MC
e_l <- quantile_ellipsoid(cm, 0.1)
e_u <- quantile_ellipsoid(cm, 0.9)
sum(in_space(e_l, z))/N_MC
sum(in_space(e_u, z))/N_MC
omega <- e_l
omega@i_ax/isqrtm(cm)
nms2 <- apply(omega@i_ax %*% z, 2, function(v) sum(v^2))
plot(nms, nms2)
nms[1:10]
nms2[1:10]
z[, 2]
isqrtm(cm) %*% z[, 2]
omega@i_ax %*% z[, 2]

## product betas
als <- c(1, 2, 3)
bts <- c(3, 2, 1)
pb1 <- new("product_in_cube", domain = cb, alphas = als, betas = bts)
xx <- sample_points(pb1, 10)
xx

de <- density_at(pb1, x)
sum(de)/N_MC * volume(cb)

de <- density_at(pb1, sample_points(pb1, N_MC))
sum(1/de)/(N_MC * volume(cb))
