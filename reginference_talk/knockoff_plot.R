library(reginference)
library(knockoff)
library(lars)
data(prostate)
x <- prostate[, 1:8]
y <- prostate[, 9]
x2 <- knockoff.create(x)
dim(x2)
colnames(x2) <- paste("K_", colnames(x), sep = "")
head(x2)
res <- lars(as.matrix(cbind(x, x2)), y)
plot(res, xvar = "norm")
res


xvar <- "norm"
breaks <- TRUE 
plottype <- "coefficients"
omit.zeros <- TRUE
eps <- 1e-10
object <- res
coef1 <- object$beta
coef1 <- betabreaker(object)
stepid = trunc(as.numeric(dimnames(coef1)[[1]]))
coef1 <- scale(coef1, FALSE, 1/object$normx)
if (omit.zeros) {
  c1 <- drop(rep(1, nrow(coef1)) %*% abs(coef1))
  nonzeros <- c1 > eps
  cnums <- seq(nonzeros)[nonzeros]
  coef1 <- coef1[, nonzeros, drop = FALSE]
}
s1 <- switch(xvar, norm = {
  s1 <- apply(abs(coef1), 1, sum)
  s1/max(s1)
}, df = object$df, arc.length = cumsum(c(0, object$arc.length)), 
step = seq(nrow(coef1)) - 1)
xname <- switch(xvar, norm = "|beta|/max|beta|", df = "Df", 
                arc.length = "Arc Length", step = "Step")
kk <- 10
matplot(res$lambda[1:kk], coef1[1:kk, ], xlab = expression(lambda), type = "o", pch = ".", 
        ylab = "", col = c(rep("blue", 8), rep("red", 8)), lty = 1, cex = 3, cex.lab = 2)
text(5.55, 1.96, "lcavol", col = "blue")
text(5.00, 1.18, "K_lcavol", col = "red")
text(2.78, 1.09, "svi", col = "blue")
text(2.27, 0.78, "lweight", col = "blue")
text(1.47, -0.15, "pgg45", col = "blue")
text(0.57, 0.89, "K_lbph", col = "red")
text(0.54, -0.63, "K_age", col = "red")
help(matplot)


locator(1)
