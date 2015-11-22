####
##  Minimum coefficient of variation for log-concave distribution on R+
####

CoV <- function(fvec, naive = FALSE) {
  if (naive) {
    ys <- sample(xs, size = 1e5, replace = TRUE, prob = exp(-fvec))
    return(mean(ys)/sd(ys))
  }
  e1 <- sum(xs * exp(-fvec))/sum(exp(-fvec))
  e2 <- sum(xs^2 * exp(-fvec))/sum(exp(-fvec))
  e1/sqrt(e2 - e1^2)
}

ef <- function(fvec) {
  sum(xs * exp(-fvec))/sum(exp(-fvec))
}
e2f <- function(fvec) {
  sum(xs^2 * exp(-fvec))/sum(exp(-fvec))
}
varf <- function(fvec) {
  e1 <- sum(xs * exp(-fvec))/sum(exp(-fvec))
  e2 <- sum(xs^2 * exp(-fvec))/sum(exp(-fvec))
  e2 - e1^2  
}
d_ef <- function(fvec, evec, naive = FALSE) {
  if (naive) {
    return(ef(fvec + evec) - ef(fvec))
  }
  e1 <- ef(fvec)
  -sum(evec * (xs-e1) * exp(-fvec))/sum(exp(-fvec))
}

d_varf <- function(fvec, evec, naive = FALSE) {
  if (naive) {
    return(varf(fvec + evec) - varf(fvec))
  }
  e1 <- ef(fvec); e2 <- e2f(fvec)
  -sum(evec * (xs^2 - 2*xs * e1 - e2 + 2*e1^2) * exp(-fvec))/sum(exp(-fvec))
}

d_CoV <- function(fvec, evec, naive = FALSE) {
  if (naive) {
    return(CoV(fvec + evec) - CoV(fvec))
  }
  e1 <- ef(fvec); e2 <- e2f(fvec); vv <- varf(fvec)
  pf <- exp(-fvec)/sum(exp(-fvec))
  sum(pf * evec * (
    (e1 - xs)/sqrt(vv) - e1/2 * vv^(-3/2) * (e2 - 2*e1^2 + 2*e1*xs - xs^2)
    ))
}

grad_CoV <- function(fvec) {
  e1 <- ef(fvec); e2 <- e2f(fvec); vv <- varf(fvec)
  pf <- exp(-fvec)/sum(exp(-fvec))
  pf * ((e1 - xs)/sqrt(vv) - e1/2 * vv^(-3/2) * (e2 - 2*e1^2 + 2*e1*xs - xs^2))
}


check_convex <- function(fvec) {
  diffs <- fvec[c(-1, -2)] + fvec[c(-(reso-1), -reso)] - 2 * fvec[c(-1, -reso)]
  min(diffs) >= -1e-10
}

reso <- 1e4
xs <- 1:reso/reso

fvec <- 5 * xs

CoV(5 * xs, TRUE)
CoV(5 * xs)
CoV(10 * xs, TRUE)

evec <- 1e-2 * rnorm(reso)

d_ef(fvec, evec, TRUE)
d_ef(fvec, evec)
d_varf(fvec, evec, TRUE)
d_varf(fvec, evec)
d_CoV(fvec, evec, TRUE)
d_CoV(fvec, evec)

plot(exp(-fvec-evec), exp(-fvec)*(1-evec), type = "l")
abline(0, 1, col = "red")

## gradient for exponential case
lmbda <- 1/10
fvec <- xs/lmbda
gr <- grad_CoV(fvec)
plot(xs, reso * gr, type = "l")
lines(xs, (1/lmbda)^2 * exp(-xs/lmbda) * (.5/lmbda * xs^2 - 2 * xs + lmbda), col = "blue")
abline(0, 0)
CoV(fvec)
res <- optimise(function(eps) CoV(fvec - eps * gr), c(0, 1000))
res$objective - CoV(fvec)
res$minimum
fvec2 <- fvec - res$minimum * gr
plot(xs, fvec2 , type = "l")
plot(xs, exp(-fvec2), type = "l")
CoV(fvec2)
