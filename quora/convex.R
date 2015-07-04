####
## Illustration of duality
####

xs <- -50:50/10
fx <- (xs - 2)^2 - 1
gx <- xs
par("mar") # 5.1 4.1 4.1 2.1
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(gx, fx, type = "l", ylim = c(-2, 10), xlim = c(-1, 5),
     xlab = expression(g(x)), ylab = expression(f(x)), axes = F, cex.lab = 2, lwd = 2)
polygon(c(gx, 5, -5), c(fx, 20, 20), density = 6, angle = 90, col = grey(0.2))
points(gx[xs == 0], fx[xs == 0], cex = 2, col = "red", lwd = 2)
abline(0, 0, lwd = 2)
abline(v = 0, lwd = 2)

plot(gx, fx, type = "l", ylim = c(-2, 10), xlim = c(-1, 5),
     xlab = expression(g(x)), ylab = expression(f(x)), axes = F, cex.lab = 2, lwd = 2)
abline(0, 0, lwd = 2)
abline(v = 0, lwd = 2)
lines(gx, gx * 2, col = "blue", lwd = 2, lty = 2)
text(2.5, 6.5, expression(lambda*g(x)), cex = 2)

plot(gx, fx + 2 * gx, type = "l", ylim = c(-2, 10), xlim = c(-1, 5),
     xlab = expression(g(x)), ylab = expression(f(x) + lambda * g(x)), axes = F, cex.lab = 2, lwd = 2)
lines(gx, fx, lty = 3, lwd = 2)
abline(0, 0, lwd = 2)
abline(v = 0, lwd = 2)
title(expression(lambda == 2), cex.main = 2)



plot(gx, fx + 0 * gx, type = "l", ylim = c(-2, 10), xlim = c(-1, 5),
     xlab = expression(g(x)), ylab = expression(f(x) + lambda * g(x)), axes = F, cex.lab = 2, lwd = 2,
     col = rgb(0.5, 0.2, 0.2))
lines(gx, fx - 3 * gx, lwd = 2, col = rgb(0.2, 0.8, 0.2))
lines(gx, fx + 2 * gx, lwd = 2, col = rgb(0.8, 0.2, 0.2))
lines(gx, fx + 4 * gx, lwd = 2, col = rgb(0.2, 0.2, 0.2))
lines(gx, fx + 6 * gx, lwd = 2, col = rgb(0.5, 0.2, 0.2))
points(gx[xs == 0], fx[xs == 0], cex = 2, col = "red", lwd = 2)
abline(0, 0, lwd = 2)
abline(v = 0, lwd = 2)


plot(gx, fx + 0 * gx, type = "l", ylim = c(-2, 10), xlim = c(-1, 5),
     xlab = expression(g(x)), ylab = expression(f(x) + lambda * g(x)), axes = F, cex.lab = 2, lwd = 2,
     col = rgb(0.5, 0.2, 0.2, 0.5))
lines(gx, fx - 3 * gx, lwd = 2, col = rgb(0.2, 0.8, 0.2, 0.5))
lines(gx, fx + 2 * gx, lwd = 2, col = rgb(0.8, 0.2, 0.2, 0.5))
lines(gx, fx + 4 * gx, lwd = 2, col = rgb(0.2, 0.2, 0.2, 1.0))
lines(gx, fx + 6 * gx, lwd = 2, col = rgb(0.5, 0.2, 0.2, 0.5))
points(gx[xs == 0], fx[xs == 0], cex = 2, col = "red", lwd = 2)
abline(0, 0, lwd = 2)
abline(v = 0, lwd = 2)
title(expression(max[lambda]*min[x]*f(x)+lambda*g(x)), cex.main = 2)

