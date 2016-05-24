## get an intuitive feel for GRF weighting kernels

library(pracma)
library(MASS)

hh <- 5 # bandwidth for GRF
v0 <- 10 # variance for global mean

n <- 500 # number of points
p <- 2 # dimension

xs <- randn(n, p)
dd <- pdist(xs)
covm <- v0 + exp(-dd/hh)
ys <- mvrnorm(1, rep(0, n), covm)

dev.new("X11", width = 8, height = 6, noRStudioGD = TRUE)
layout(matrix(1:2, 1, 2))
par(mar = c(0, 0, 0, 0))
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
plot(xs, axes = FALSE, ann = FALSE, asp = 1)
xx <- locator(1)
while(!is.null(xx)) {
  ind <- order(colSums((t(xs) - c(xx$x, xx$y))^2))[1]
  wts <- solve(covm[-ind, -ind], covm[-ind, ind])
  print(sum(wts))
  barplot(sort(wts), ylim = c(-1/sqrt(n), 10/sqrt(n)))
  plot(xs, axes = FALSE, ann = FALSE, asp = 1, col = "white")
  for (i in 1:(n-1)) {
    if (wts[i] > 0) cl <- grey(level = 1 - pmax(0, wts[i]/max(wts)))
    if (wts[i] < 0) cl <- rgb(1 +wts[i]/max(wts), 1, 1+wts[i]/max(wts))
    points(xs[-ind, ][i, , drop = FALSE], cl = cl)
  }
  points(xs[ind, , drop = FALSE], col = "red")
  xx <- locator(1)
}
