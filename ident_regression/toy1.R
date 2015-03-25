# Toy Model

# 1. Simulation

bt <- 5            # value of beta
al <- bt           # value of alpha
sigma2_x <- 1      # variance of x
sigma2_eps <- 0.5  # variance of noise
k_classes <- 5     # number of classes (x_i)
n_points <- 20     # number of points

# generate classes and points
x_cands <- sort(rnorm(k_classes, 0, sqrt(sigma2_x)))
mu_cands <- bt * x_cands
sel_class <- sample(k_classes, n_points, TRUE)
sel_x <- x_cands[sel_class]
y_star <- mu_cands[sel_class] +
  rnorm(n_points, 0, sqrt(sigma2_eps))

# predict labels using alpha
muhat_cands <- al * x_cands
library(class)
te_classes <- knn(t(t(muhat_cands)),
                  t(t(y_star)), 1:k_classes, k = 1)

# plot of points and mean line
#  connected to true class
plot(sel_x, y_star)
lines(x_cands, mu_cands)
for (i in 1:n_points) {
  cc = "grey"
  if (te_classes[i] != sel_class[i]) cc = "red"
  lines(c(sel_x[i], x_cands[te_classes[i]]),
        c(y_star[i], mu_cands[te_classes[i]]),
        col = cc)
}




