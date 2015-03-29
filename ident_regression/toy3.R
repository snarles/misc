############
## Theory for mispecified
############


ffun <- function(v) {
  v + 1.5 * sin(6 * v)
}
xs <- rnorm(1e6)
ys <- ffun(xs)
res <- lm(ys ~ xs + 0)
bt0 <- coefficients(res)[1]
bt0


pdf("paper/toy2_plot.pdf")
xgrid <- seq(-3, 3, .01)
xs <- rep(rnorm(1000), each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1)
lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 2")
abline(0, bt0, col = "blue", lty = 3, lwd = 2)
legend(-3, 6, c("f(x)", expression(paste(x, beta[LS]))),
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


bts <- seq(0, 10, 0.5)
bts

#mc2 <- sapply(bts, function(bt) mean(simulate(bt, 2, 1e4)))
mc3 <- sapply(bts, function(bt) mean(simulate(bt, 3, 1e4)))
#mc5 <- sapply(bts, function(bt) mean(simulate(bt, 5, 1e4)))
mc10 <- sapply(bts, function(bt) mean(simulate(bt, 10, 1e4)))
mc20 <- sapply(bts, function(bt) mean(simulate(bt, 20, 1e4)))
#mc40 <- sapply(bts, function(bt) mean(simulate(bt, 40, 1e3)))
#mc80 <- sapply(bts, function(bt) mean(simulate(bt, 80, 1e3)))

pdf("paper/toy2_plot2.pdf")
xgrid <- seq(-3, 3, .01)
xs <- rep(rnorm(1000), each = 10)
ys <- ffun(xs) + rnorm(length(xs))
plot(xs, ys, xlab = "x", ylab = "y", pch = ".", cex = 1)
lines(xgrid, ffun(xgrid), lwd = 2)
title("Toy model 2")
abline(0, bt0, col = "blue", lty = 3, lwd = 2)
abline(0, 1.5, col = "red", lty = 2, lwd = 2)
abline(0, 9, col = "red", lty = 2, lwd = 3)
legend(-3, 6, 
       c("f(x)", expression(paste(x, beta[LS])), 
         expression(paste(x, beta[3])), expression(paste(x, beta[20]))),
       lty = c(1, 3, 2, 2), col = c("black", "blue", "red", "red"),
       lwd = c(2, 2, 2, 3))
dev.off()

pdf("paper/toy2_l3.pdf")
plot(bts, mc3, type = "o",
     ylim = c(0.3, 1), xlab = expression(hat(beta)), ylab = "Misclassification")
title("3 test classes")
abline(v = bt0, lty = 3, col = "blue")
abline(v = 1.5, lty = 3, col = "red")
text(0.5, 0.35, expression(hat(beta)[LS]), col = "blue", cex = 2)
text(2, 0.35, expression(hat(beta)[3]), col = "red", cex = 2)
dev.off()

pdf("paper/toy2_l20.pdf")
plot(bts, mc20, type = "o",
     ylim = c(0.3, 1), xlab = expression(hat(beta)), ylab = "Misclassification")
title("20 test classes")
abline(v = bt0, lty = 3, col = "blue")
abline(v = 9, lty = 3, col = "red")
text(0.5, 0.35, expression(hat(beta)[LS]), col = "blue", cex = 2)
text(9.5, 0.35, expression(hat(beta)[20]), col = "red", cex = 2)
dev.off()




plot(bts, mc10, type = "o")
plot(bts, mc20, type = "o")

plot(bts, mc40, type = "o")
plot(bts, mc80, type = "o")

plot(bts, mc3, type = "o")
plot(bts, mc5, type = "o")
plot(bts, mc10, type = "o")
plot(bts, mc20, type = "o")


