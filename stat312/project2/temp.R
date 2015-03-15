b1 <- new("ball", radius = 2, dimension = 3)
centers <- sample_points(b1, 1)
cm <- identity_covs(3, 1)
mb0 <- new("mixture_in_ball", domain = b1, 
           centers = centers, covariances = cm, weights = 1)
N_MONTE_CARLO <- 1e5
x <- sample_points(b1, N_MONTE_CARLO)
x <- 4 * rbind(runif(N_MONTE_CARLO), runif(N_MONTE_CARLO), runif(N_MONTE_CARLO)) - 2
x <- x[, in_space(b1, x)]
de <- density_at(mb0, x)
sum(de) * 4/3 * pi * 8 / dim(x)[2]
x <- sample_points(mb0, N_MONTE_CARLO)
de <- density_at(mb0, x)
sum(1/de) / dim(x)[2]
4/3 * pi * 8
