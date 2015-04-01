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

theory_x <- function(al, k_cl, x0, res = 30) {
  g0 <- -res:res
  z <- g0 / sqrt(res)
  d2 <- exp(- .5 * z^2 )
  d2 <- d2/sum(d2)
  eps <- sqrt(sigma2_eps) * z
  mus <- bt * x0 + eps
  sigma2 <- al^2 * sigma2_x
  thress <- (bt - al) * x0 + eps
  ps <- p_folded(mus, sigma2, thress)
  1 - sum(ps^(k_cl-1) * d2)
}

theory_e <- function(al, k_cl, eps, res = 30) {
  g0 <- -res:res
  z <- g0 / sqrt(res)
  d2 <- exp(- .5 * z^2 )
  d2 <- d2/sum(d2)
  xs <- sqrt(sigma2_x) * z
  mus <- bt * xs + eps
  sigma2 <- al^2 * sigma2_x
  thress <- (bt - al) * xs + eps
  ps <- p_folded(mus, sigma2, thress)
  1 - sum(ps^(k_cl-1) * d2)
}

## simplified form
theory3 <- function(al, k_cl, res = 30) {
  del <- al - bt
  g0 <- -res:res
  z2 <- cbind(rep(g0, 2*res + 1),
              rep(g0, each = 2*res + 1)) /
    sqrt(res)
  d2 <- exp(-.5 * z2[,1]^2 - .5 * z2[,2]^2 )
  d2 <- d2/sum(d2)
  xs <- sqrt(sigma2_x) * z2[,1]
  eps <- sqrt(sigma2_eps) * z2[,2]
  ps <- 1 - abs(pnorm(xs/sqrt(sigma2_x)) - 
                  pnorm((xs * (bt - del) + 2 * eps)/(sqrt(sigma2_x) * (bt + del))))
  1 - sum(ps^(k_cl-1) * d2)
}

## taylor expansion
theory4 <- function(al, k_cl, res = 30) {
  del <- al - bt
  sigma_x <- sqrt(sigma2_x)
  g0 <- -res:res
  z2 <- cbind(rep(g0, 2*res + 1),
              rep(g0, each = 2*res + 1)) /
    sqrt(res)
  d2 <- exp(-.5 * z2[,1]^2 - .5 * z2[,2]^2 )
  d2 <- d2/sum(d2)
  xs <- sqrt(sigma2_x) * z2[,1]
  eps <- sqrt(sigma2_eps) * z2[,2]
  ps <- 1 - abs(
                pnorm(xs/sigma_x) - 
                pnorm((xs/sigma_x) + (2 * eps)/(bt * sigma_x) 
                      - del * (2 * xs/(sigma_x * bt) + 2 * eps/bt^2))
                )
  1 - sum(ps^(k_cl-1) * d2)
}


library(class)

bt <- 2
sigma2_x <- 1
sigma2_eps <- 1
ps <- simulate(2, 3, 10000)
mean(ps)
sd(ps)/sqrt(1e5)
theory1(bt, 3, 400)
theory3(bt + 1e-2, 3, 400)
theory3(bt - 1e-2, 3, 400)
theory4(bt + 1e-2, 3, 400)
theory4(bt - 1e-2, 3, 400)


theory1(2, 2, 400)
theory1(2 + 1e-1, 2, 400)

k_cl <- 2
theory_x(bt, k_cl, 1, 400)
theory_x(bt + 1e-2, k_cl, 1, 400)
theory_x(bt - 1e-2, k_cl, 1, 400)

eps <- 2
theory_e(bt, k_cl, eps, 400)
theory_e(bt + 1e-2, k_cl, eps, 400)
theory_e(bt - 1e-2, k_cl, eps, 400)


theory1(bt, k_cl, 400)
theory1(bt + 1e-2, k_cl, 400)
theory1(bt - 1e-2, k_cl, 400)



k_cl <- 10
theory1(bt, k_cl, 400)
sapply(bt + .01 * 0:3, function(x) {theory1(x, k_cl, 400)})
sapply(bt - .01 * 0:3, function(x) {theory1(x, k_cl, 400)})

theory1(bt, 100)
theory1(bt + .01, 100)
theory1(bt - .01, 100)

theory2(bt)
theory2(bt + .01)
theory2(bt - .01)

k_cl <- 3

bts <- (-50:50)/10 + 1e-3
res <- length(bts)
rmat <- matrix(0, res, res)
for (i in 1:res) {
  for (j in 1:res) {
    bt <- bts[i]
    rmat[i,j] <- theory1(bts[j], k_cl)
  }
}


sigma2_x <- 1
sigma2_eps <- 1
bt <- 1e-5
r0 <- sapply(bts, function(v) theory1(v, k_cl))
pdf("paper/risk_0.pdf")
plot(bts, r0, type = "l", ylim = c(0, 1), xlab = expression(hat(beta)), ylab = "R")
abline(v = bt, lty = 2)
#title(expression(beta==0))
dev.off()

bt <- 1
r0 <- sapply(bts, function(v) theory1(v, k_cl))
pdf("paper/risk_1.pdf")
plot(bts, r0, type = "l", ylim = c(0, 1), xlab = expression(hat(beta)), ylab = "R")
abline(v = bt, lty = 2)
#title(expression(beta==1))
dev.off()

bt <- 2
r0 <- sapply(bts, function(v) theory1(v, k_cl))
pdf("paper/risk_2.pdf")
plot(bts, r0, type = "l", ylim = c(0, 1), xlab = expression(hat(beta)), ylab = "R")
abline(v = bt, lty = 2)
#title(expression(beta==2))
dev.off()

pdf("paper/rmat.pdf")
filled.contour(bts, bts, rmat, xlab = expression(hat(beta)), ylab = expression(beta))
title(expression(paste("R(", beta, "; ", hat(beta), ")")))
dev.off()

bt <- 1
pdf("paper/toy_est.pdf")
wt <- dnorm((bts - bt))
lala <- wt %*% rmat/sum(wt)
plot(bts, lala, type="l", ylab = "Bayes risk", xlab = expression(hat(beta)))
title("Bayes estimation")
abline(v = 1, lty = 3, lwd = 2, col = "blue")
bth = bts[lala == min(lala)]
abline(v = bth, lty = 2)
points(bts[order(lala)[1]], min(lala))
legend(-5, .55, c(expression(hat(beta)[MAP]), expression(hat(beta))), lty = c(3, 2), lwd = c(2, 1), col = c("blue", "black"))
dev.off()