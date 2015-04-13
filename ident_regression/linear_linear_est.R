############
## Estimation in the linear/linear case
############

## empirical misclassification rate

library(parallel)
library(class)

simulate <- function(bt, sigma2_x, sigma2_eps,
                     al, k_cl, n_trials, seed = 0, mc.cores = 25) {
  simulate0 <- function(seed) {
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
    return(mrs)    
  }
  if (length(seed) == 1) {
    return(simulate0(seed))
  }
  else {
    return(mclapply(seed, simulate0, mc.cores = mc.cores))
  }
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
theory1 <- function(bt, sigma2_x, sigma2_eps,
                    al, k_cl, res = 30) {
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


bt <- 2
sigma2_x <- 1
sigma2_eps <- 1
ps <- simulate(bt, sigma2_x, sigma2_eps, 2, 3, 10000, 1:25)
ps <- unlist(ps)
mean(ps)
sd(ps)/sqrt(length(ps))
theory1(bt, sigma2_x, sigma2_eps, bt, 3, 400)

bts <- (-200:200)/20 + 1e-3
k_cl <- 4
res <- length(bts)
iloop1 <- function(i) {
  ans <- numeric(res)
  for (j in 1:res) {
    ans[j] <- theory1(bts[i], sigma2_x, sigma2_eps, bts[j], k_cl, 100)
  }
  ans
}

proc.time()
temp <- mclapply(1:res, iloop1, mc.cores = 26)
proc.time()

sapply(temp, length)
rmat <- do.call(rbind, temp)

#pdf("paper/rmat.pdf")
filled.contour(bts, bts, rmat, xlab = expression(hat(beta)), ylab = expression(beta))
title(expression(paste("R(", beta, "; ", hat(beta), ")")))
#dev.off()

## Optimal beta hat given the prior
opt_bth <- function(mu, sigma2) {
  dn <- dnorm(bts, mean = mu, sd = sqrt(sigma2))
  dn <- dn/sum(dn)
  lala <- dn %*% rmat
  bth = bts[lala == min(lala)]
  bth
}

sigma2 <- 0.5
cand_bts <- (-200:200)/40 + 1e-3
temp1 <- function(i) {
  opt_bth(cand_bts[i], sigma2)
}
#temp1(1)
res <- sapply(1:length(cand_bts), temp1)
plot(cand_bts, res, type = 'l')
lines(cand_bts, cand_bts, lty = 2)
title(expression(paste(sigma^2, "=")))
title(paste("        ", sigma2))

## minimal estimate phenomenon

min(abs(res))
## sigma2 = 10,  3.3
##          5,   2.55
##          0.5, 1.1

max(rmat)
l1n <- function(v) v/sum(v)
sds <- c(0.25, .3, .5, 1, 2, 3)
range(bts)
#plot(NA, NA, xlim = range(bts), ylim = c(0, 1),
#     xlab = expression(beta), ylab = "R")
layout(matrix(1:length(sds), length(sds), 1))
oldmar <- par("mar")
par(mar = c(0, 0, 0, 0))
for (i in 1:length(sds)) {
  y <- l1n(dnorm(bts, mean = 0, sd = sds[i])) %*% rmat
  plot(bts, y,
        type = 'l', col = rainbow(length(sds))[i])  
}
