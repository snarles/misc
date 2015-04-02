############
## Theory for mispecified
############


bt <- 1
ffun <- function(v) {
  bt * v
}
xs <- runif(1e6) - .5
ys <- ffun(xs)
sigma2_x <- 1
sigma2_eps <- 1
library(class)

pdf("paper/toy3c_plot.pdf")
xgrid <- seq(-3, 3, .01)
xs <- rep(runif(1000) - .5, each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10))
lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 1")
abline(0, bt, lwd = 2)
dev.off()

## empirical misclassification rate
simulate <- function(al, k_cl, n_trials, seed = 0) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
    xs <- 3 * (runif(k_cl) - .5)
    ys <- ffun(xs) + rnorm(k_cl, 0, sqrt(sigma2_eps))
    yhats <- xs * al
    te_cl <- knn(t(t(yhats)), t(t(ys)), 1:k_cl, k = 1)
    mr <- sum(te_cl != 1:k_cl)/k_cl
    mrs[i] <- mr
  }
  mrs 
}


bts <- seq(0.99, 1.01, 0.005)
bts

library(parallel)
mc3 <- mclapply(
    1:30,
    function(sed) {
        sapply(bts, function(bt) mean(simulate(bt, 3, 1e5, sed)))
    },
    mc.cores = 30)
mc3 <- as.matrix(data.frame(mc3))
apply(mc3, 1, mean)
mean(mc3[2, ] - mc3[3, ])
sd(mc3[2, ] - mc3[3, ])/sqrt(30 * 1e5)


mc20 <- sapply(bts, function(bt) mean(simulate(bt, 20, 1e6, sed)))

#mc5 <- sapply(bts, function(bt) mean(simulate(bt, 5, 1e4)))
#mc10 <- sapply(bts, function(bt) mean(simulate(bt, 10, 1e4)))

mc40 <- sapply(bts, function(bt) mean(simulate(bt, 40, 1e3)))
#mc80 <- sapply(bts, function(bt) mean(simulate(bt, 80, 1e3)))
min3 <-bts[mc3 == min(mc3)]
min20 <-bts[mc20 == min(mc20)]
min40 <-bts[mc40 == min(mc40)]

min3

#min2 <-bts[mc3 == min(mc3)]

pdf("paper/toy3c_plot.pdf")
xgrid <- seq(-2, 2, .01)
xs <- 3 * rep(runif(1000) - .5, each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-4, 4))
#lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 1: Uniform")
abline(0, bt, col = "blue", lty = 3, lwd = 2)
abline(0, min3, col = "red", lty = 2, lwd = 2)
abline(0, min20, col = "red", lty = 2, lwd = 3)
legend(1, -2.5, 
       c(expression(paste(x, beta)), 
         expression(paste(x, beta[3])), expression(paste(x, beta[20]))),
       lty = c(3, 2, 2), col = c("blue", "red", "red"),
       lwd = c(2, 2, 3))
dev.off()

min2 <-bts[mc2 == min(mc2)]
plot(bts, mc2, type = "o",
     ylim = c(0, 1), xlab = expression(hat(beta)), ylab = "Misclassification")
abline(v = min2, lty = 3, col = "red")


pdf("paper/toy3a_l3.pdf")
plot(bts, mc3, type = "o",
     ylim = c(0.3, 1), xlab = expression(hat(beta)), ylab = "Misclassification")
title("3 test classes")
#abline(v = bt0, lty = 3, col = "blue")
abline(v = min3, lty = 3, col = "red")
#text(bt0 + 0.5, 0.35, expression(hat(beta)[LS]), col = "blue", cex = 2)
text(min3 - 0.1, 0.35, expression(hat(beta)[3]), col = "red", cex = 2)
dev.off()

pdf("paper/toy3a_l20.pdf")
min20 <-bts[mc20 == min(mc20)]
plot(bts, mc20, type = "o",
     ylim = c(0.3, 1), xlab = expression(hat(beta)), ylab = "Misclassification")
title("20 test classes")
abline(v = bt0, lty = 3, col = "blue")
abline(v = min20, lty = 3, col = "red")
#text(0.5, 0.35, expression(hat(beta)[LS]), col = "blue", cex = 2)
text(min20 - 0.1, 0.35, expression(hat(beta)[20]), col = "red", cex = 2)
dev.off()




plot(bts, mc10, type = "o")
plot(bts, mc20, type = "o")

plot(bts, mc40, type = "o")
plot(bts, mc80, type = "o")

plot(bts, mc3, type = "o")
plot(bts, mc5, type = "o")
plot(bts, mc10, type = "o")
plot(bts, mc20, type = "o")

## loss functions


dels <- -100:100/100

pdf("paper/loss_se.pdf")
plot(dels, dels^2, ylab = "loss", xlab = expression(y-hat(y)), type = "l")
#title("Squared Error loss")
dev.off()

l2 <- 1-(1 - pnorm(sqrt(abs(dels))) + pnorm(-sqrt(abs(dels))))
l3 <- 1-(1 - pnorm(sqrt(abs(dels))) + pnorm(-sqrt(abs(dels))))^2
l5 <- 1-(1 - pnorm(sqrt(abs(dels))) + pnorm(-sqrt(abs(dels))))^4
l10 <- 1-(1 - pnorm(sqrt(abs(dels))) + pnorm(-sqrt(abs(dels))))^9
l20 <- 1-(1 - pnorm(sqrt(abs(dels))) + pnorm(-sqrt(abs(dels))))^19

pdf("paper/loss_2.pdf")
plot(dels, l2, ylab = "loss", xlab = expression(y-hat(y)), type = "l")
#title("Identification loss, L = 2")
dev.off()

pdf("paper/loss_3.pdf")
plot(dels, l3, ylab = "loss", xlab = expression(y-hat(y)), type = "l")
#title("Identification loss, L = 3")
dev.off()

pdf("paper/loss_10.pdf")
plot(dels, l10, ylab = "loss", xlab = expression(y-hat(y)), type = "l")
#title("Identification loss, L = 10")
dev.off()

pdf("paper/loss_20.pdf")
plot(dels, l20, ylab = "loss", xlab = expression(y-hat(y)), type = "l")
#title("Identification loss, L = 20")
dev.off()




xgrid <- seq(-3, 3, .01)
xs <- rep(rnorm(1000), each = 10)
ys <- ffun(xs) + rnorm(length(xs))

pdf("paper/plot_loss_sq.pdf")
plot(xs, ys, xlab = "", ylab = "", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10), axes = FALSE)
#lines(xgrid, ffun(xgrid), lwd = 2)
#title("Toy model 2")
abline(0, bt0, col = "blue", lty = 3, lwd = 3)
dev.off()

pdf("paper/plot_loss_3.pdf")
plot(xs, ys, xlab = "", ylab = "", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10), axes = FALSE)
#lines(xgrid, ffun(xgrid), lwd = 2)
#title("Toy model 2")
abline(0, min3, col = "red", lty = 2, lwd = 3)
dev.off()

pdf("paper/plot_loss_20.pdf")
plot(xs, ys, xlab = "", ylab = "", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10), axes = FALSE)
#lines(xgrid, ffun(xgrid), lwd = 2)
#title("Toy model 2")
abline(0, min20, col = "red", lty = 2, lwd = 3)
dev.off()
