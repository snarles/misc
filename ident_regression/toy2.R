############
## Theory
############


## empirical misclassification rate
simulate <- function(al, k_cl, n_trials, seed = 0) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
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
theory1 <- function(al, k_cl, res = 30) {
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

## quantity derived from theory 1
theory2 <- function(al, res = 30) {
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
  ps1 <- 1 + sign(thress) * pnorm(xs / sqrt(sigma2_x))
  ps2 <- sign(thress) * 
     pnorm(((-2*bt + al)*xs - 2*eps)/(al * sqrt(sigma2_x)))
  ps <- ps1 + ps2
  c(1 - sum(ps * d2), sum(ps1 * d2), sum(ps2 * d2))
}



library(class)

bt <- 100
sigma2_x <- 1
sigma2_eps <- 1
#mean(simulate(2, 3, 1000))
k_cl <- 10
sapply(bt + .001 * 0:10, function(x) {theory1(x, k_cl)})
sapply(bt - .001 * 0:10, function(x) {theory1(x, k_cl)})

theory1(bt, 100)
theory1(bt + .01, 100)
theory1(bt - .01, 100)

theory2(bt)
theory2(bt + .01)
theory2(bt - .01)
