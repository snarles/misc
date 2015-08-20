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
  ans <- c()
  mult <- 1/cauchy_mult(delta)
  while (length(ans) < n) {
    xs <- rcauchy(n)
    probs <- mult * exp(-(abs(xs)^delta))/(1 + xs^2)
    filt <- (probs > runif(n))
    ans <- c(ans, xs[filt])
  }
  ans <- ans[1:n]
  ans
}

exp_samp <- function(delta, n) {
  ans <- c()
  mult <- 1/exp_mult(delta)
  while (length(ans) < n) {
    xs <- rexp(n)
    probs <- mult * exp(-(abs(xs)^delta - xs))
    filt <- (probs > runif(n))
    ans <- c(ans, xs[filt])
  }
  ans <- ans[1:n]
  ans <- ans * sign(runif(n) - .5)
  ans
}

#eta_function(2) - sqrt(pi)
#eta_function(1) - 2

## delta < 1: Rejection sampling via Cauchy
xs <- cauchy_samp(0.5, 1000)
hist(xs, breaks=40)

## delta < 2: Rejection sampling via exponential
xs <- exp_samp(1.5, 1000)
hist(xs, breaks=40)

####
##  Partition functions
####

betas <- 1/seq(0.01, 2, 0.01)

Eis <- function(delta, N) {
  if (delta < 1) {
    Ei <- N^((delta-1)/(delta)) * cauchy_samp(delta, 2 ^ N)    
  } else if (delta == 1) {
    Ei <- rexp(2 ^ N)    
  } else if (delta < 2) {
    Ei <- N^((delta-1)/(delta)) * exp_samp(delta, 2^N)    
  } else if (delta == 2) {
    Ei <- N^((delta-1)/(delta)) * rnorm(2^N)
  }
  Ei
}

phis <- function(delta, N, betas) {
  phi <- function(beta) (1/N) * logsumexp(-beta * Ei)
  Ei <- Eis(delta, N)
  phis <- sapply(betas, phi)
  phis
}

triplot <- function(delta, betas, yl = c(-10, 10), Ns) {
  plot(1/betas, -phis(delta, Ns[1], betas)/betas, type = "l", ylim = yl)
  for (N in Ns) {
    lines(1/betas, -phis(delta, N, betas)/betas,
          lwd = 3 * N/max(Ns))
  }
}

phiplot <- function(delta, betas, yl = c(-10, 10), Ns) {
  plot(betas, phis(delta, Ns[1], betas), type = "l", ylim = yl)
  for (N in Ns) {
    lines(betas, phis(delta, N, betas),
          lwd = 3 * N/max(Ns))
  }
}


Ns <- c(5, 10, 15)

triplot(0.5, 1/seq(0.01, 0.5, 0.01), c(-1, 1), Ns)
triplot(1, betas, c(-1, 1), Ns)
triplot(1.5, betas, c(-1, 0), Ns)
triplot(2, betas, c(-2, 0), Ns)


phiplot(0.5, seq(0, 2, 0.01), c(-1, 1), Ns)
phiplot(1, seq(0, 2, 0.01), c(-1, 1), Ns)
phiplot(1.5, seq(0, 2, 0.01), c(0, 2), Ns)
phiplot(2, seq(0, 2, 0.01), c(0, 2), Ns)
