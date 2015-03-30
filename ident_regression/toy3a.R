############
## Theory for mispecified
############


ffun <- function(v) {
  3*v - (v/1.2)^5
}
xs <- rnorm(1e6)
ys <- ffun(xs)
res <- lm(ys ~ xs + 0)
bt0 <- coefficients(res)[1]
bt0
sigma2_x <- 1
sigma2_eps <- 1
library(class)

pdf("paper/toy3a_plot.pdf")
xgrid <- seq(-3, 3, .01)
xs <- rep(rnorm(1000), each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10))
lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 2")
abline(0, bt0, col = "blue", lty = 3, lwd = 2)
legend(-2, 9, c("f(x)", expression(paste(x, beta[LS]))),
       lty = c(1, 3), col = c("black", "blue"), lwd = c(2, 2))
dev.off()

## empirical misclassification rate
simulate <- function(al, k_cl, n_trials, seed = 0) {
  set.seed(seed)
  mrs <- numeric(n_trials)
  for (i in 1:n_trials) {
    xs <- rnorm(k_cl, 0, sqrt(sigma2_x))
    ys <- ffun(xs) + rnorm(k_cl, 0, sqrt(sigma2_eps))
    yhats <- xs * al
    te_cl <- knn(t(t(yhats)), t(t(ys)), 1:k_cl, k = 1)
    mr <- sum(te_cl != 1:k_cl)/k_cl
    mrs[i] <- mr
  }
  mrs 
}


bts <- seq(0, 3, 0.1)
bts

#mc2 <- sapply(bts, function(bt) mean(simulate(bt, 2, 1e4)))
mc3 <- sapply(bts, function(bt) mean(simulate(bt, 3, 1e4)))
#mc5 <- sapply(bts, function(bt) mean(simulate(bt, 5, 1e4)))
#mc10 <- sapply(bts, function(bt) mean(simulate(bt, 10, 1e4)))
mc20 <- sapply(bts, function(bt) mean(simulate(bt, 20, 1e4)))
mc40 <- sapply(bts, function(bt) mean(simulate(bt, 40, 1e3)))
#mc80 <- sapply(bts, function(bt) mean(simulate(bt, 80, 1e3)))
min3 <-bts[mc3 == min(mc3)]
min20 <-bts[mc20 == min(mc20)]
min40 <-bts[mc40 == min(mc40)]

min3

#min2 <-bts[mc3 == min(mc3)]

pdf("paper/toy3a_plot2.pdf")
xgrid <- seq(-3, 3, .01)
xs <- rep(rnorm(1000), each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1, xlim = c(-2, 2), ylim = c(-10, 10))
lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 2")
abline(0, bt0, col = "blue", lty = 3, lwd = 2)
abline(0, min3, col = "red", lty = 2, lwd = 2)
abline(0, min20, col = "red", lty = 2, lwd = 3)
legend(-2, 9, 
       c("f(x)", expression(paste(x, beta[LS])), 
         expression(paste(x, beta[3])), expression(paste(x, beta[20]))),
       lty = c(1, 3, 2, 2), col = c("black", "blue", "red", "red"),
       lwd = c(2, 2, 2, 3))
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
