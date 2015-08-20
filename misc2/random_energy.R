####
## Random Energy Model
####

library(rootSolve)

delta <- 2

logsumexp <- function(v) log(sum(exp(v - max(v)))) + max(v)

eta_function <- function(delta, quad_wid = 1e3, quad_res = 1e-4) {
  quad_xs <- seq(-quad_wid, quad_wid, quad_res)
  exp(logsumexp(-(abs(quad_xs) ^ delta))) * quad_res  
}

cauchy_mult <- function(delta) {
  ratio <- function(x) exp(x^delta)/(1 + x^2)
  f <- function(x) (1 + x^2) * delta * x^(delta - 1) - 2 * x
  rts <- uniroot.all(f, c(1e-3, 1e2))
  1/min(sapply(rts, ratio))
}

exp_mult <- function(delta) {
  x <- (1/delta) ^ (1/(delta - 1))
  1/exp(-(x - x ^ delta))
}

cauchy_samp <- function(delta, n) {
  
}

exp_samp <- function(delta, n) {
  
}

#eta_function(2) - sqrt(pi)
#eta_function(1) - 2


## delta < 1: Rejection sampling via Cauchy
delta <- 0.5
eta <- eta_function(delta)
xs <- seq(0, 10, 0.1)
plot(xs, exp(-(xs ^ delta))/eta, type = "l", col = "red")
lines(xs, pi * cauchy_mult(delta)/eta * dcauchy(xs))

## delta < 2: Rejection sampling via exponential
delta <- 1.5
eta <- eta_function(delta)
xs <- seq(0, 10, 0.1)
plot(xs, exp(-(xs ^ delta))/eta, type = "l", col = "red")
lines(xs, exp_mult(delta)/eta * exp(-xs))



