# Toy Model

# 1. Simulation

bt <- 5            # value of beta
sigma2_x <- 1      # variance of x
sigma2_eps <- 0.5  # variance of noise
k_classes <- 5     # number of classes (x_i)
n_points <- 20     # number of points

x_cands <- sort(rnorm(k_classes, 0, sqrt(sigma2_x)))
mu_cands <- bt * x_cands
sel_class <- sample(k_classes, n_points, TRUE)
sel_x <- x_cands[sel_class]
y_star <- mu_cands[sel_class] +
  rnorm(n_points, 0, sqrt(sigma2_eps))

# plot of points and mean line
plot(sel_x, y_star)
lines(x_cands, mu_cands)




