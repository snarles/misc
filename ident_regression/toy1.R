## Toy Model

################
## 1. Simulation
################

bt <- 5            # value of beta
al <- bt           # value of alpha
sigma2_x <- 1      # variance of x
sigma2_eps <- 0.5  # variance of noise
k_classes <- 5     # number of classes (x_i)
n_points <- 20     # number of points

## generate classes and points
x_cands <- sort(rnorm(k_classes, 0, sqrt(sigma2_x)))
mu_cands <- bt * x_cands
sel_class <- sample(k_classes, n_points, TRUE)
sel_x <- x_cands[sel_class]
y_star <- mu_cands[sel_class] +
  rnorm(n_points, 0, sqrt(sigma2_eps))

## predict labels using alpha
muhat_cands <- al * x_cands
library(class)
te_classes <- knn(t(t(muhat_cands)),
                  t(t(y_star)), 1:k_classes, k = 1)

## plot of points and mean line
##  connected to true class
plot(sel_x, y_star, xlab = "x", ylab = "y")
lines(x_cands, mu_cands)
for (i in 1:n_points) {
  cc = "grey"
  if (te_classes[i] != sel_class[i]) cc = "red"
  lines(c(sel_x[i], x_cands[te_classes[i]]),
        c(y_star[i], mu_cands[te_classes[i]]),
        col = cc)
}

############
## 2. Theory
############

bt <- 2
sigma2_x <- 0.8
sigma2_eps <- 0.5
k_classes <- 1e6
n_points <- 1e6
x_cands <- sort(rnorm(k_classes, 0, sqrt(sigma2_x)))
mu_cands <- bt * x_cands
#sel_class <- sample(k_classes, n_points, TRUE)
sel_class <- 1:k_classes
sel_x <- x_cands[sel_class]
y_star <- mu_cands[sel_class] +
  rnorm(n_points, 0, sqrt(sigma2_eps))

## covariance of x and y
var(y_star)
bt^2 * sigma2_x + sigma2_eps
cov(sel_x, y_star)
bt * sigma2_x

## conditional distribution of x | y
cond_coef <- bt * sigma2_x/(bt^2 * sigma2_x + sigma2_eps)
cond_var <- sigma2_x * (1- bt * cond_coef)
temp_o <- order(y_star)
y_star <- y_star[temp_o]
sel_x <- sel_x[temp_o]
cond_x <- cond_coef * y_star
rand_range <- sample(n_points, 1) + (-100:100)
mean(sel_x[rand_range])
mean(cond_x[rand_range])
var(sel_x[rand_range])
cond_var

## distribution of al * x  - y
al <- 1.8
alxy <- al * sel_x - y_star
mu_alxy <- (al * cond_coef - 1) * y_star
sigma2_y <- bt^2 * sigma2_x + sigma2_eps
var_alxy <- al^2 * cond_var

rand_range <- sample(n_points, 1) + (-100:100)
mean(alxy[rand_range])
mean(mu_alxy[rand_range])
var(alxy[rand_range])
var_alxy

## empirical misclassification rate
simulate <- function(al, k_cl, n_trials, seed = 0) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
#    cl <- sort(sample(k_classes, k_cl))
#    xs <- sel_x[cl]
#    ys <- y_star[cl]
    xs <- rnorm(k_cl, 0, sqrt(sigma2_x))
    ys <- bt * xs + rnorm(k_cl, 0, sqrt(sigma2_eps))
    yhats <- xs * al
    te_cl <- knn(t(t(yhats)), t(t(ys)), 1:k_cl, k = 1)
    mr <- sum(te_cl != 1:k_cl)/k_cl
    mrs[i] <- mr
  }
  mrs 
}

## probability that |N(mu, sigma2)| > thres
p_folded <- function(mu, sigma2, thres, empirical = FALSE) {
  mu_conv <- mu/sqrt(sigma2)
  thres_conv <- abs(thres)/sqrt(sigma2)
  if (empirical) {
    return(sum(abs(rnorm(1e6, mu, sqrt(sigma2))) < thres)/1e6)
  }
  1 - pnorm(-mu_conv + thres_conv) + pnorm(-mu_conv - thres_conv)
}

## misclassification rate based on conditional dist
theory1 <- function(al, k_cl, res = 10) {
  # normal distribution grid
  g0 <- -res:res
  z2 <- cbind(rep(g0, 2*res + 1),
              rep(g0, each = 2*res + 1)) /
              sqrt(res)
  d2 <- exp(-.5 * z2[,1]^2 - .5 * z2[,2]^2 )
  d2 <- d2/sum(d2)
  xs <- sqrt(sigma2_x) * z2[,1]
  eps <- sqrt(sigma2_eps) * z2[,2]
  mus <- bt * xs + eps
  sigma2 <- al^2 * sigma2_x
  thress <- (bt - al) * xs + eps
  ps <- p_folded(mus, sigma2, thress)
  1 - sum(ps^(k_cl-1) * d2)
}

mean(simulate(2, 3, 1000))
theory1(1.999, 3, 30)



