a <- list(); b <- list()
a[[1]] <- matrix(c(0,0,0,.16),2,2)
a[[2]] <- matrix(c(.2,.23,-.26,.22),2,2)
a[[3]] <- matrix(c(-.15,.26,.28,.24),2,2)
a[[4]] <- matrix(c(.85,-.04,.04,.85),2,2)
b <- list(c(0,0),c(0,1.6),c(0,.44),c(0,.16))
p <- c(.01,.07,.07,.85)

reps <- 10000
xs <- numeric()
x <- c(0,0)
for (i in 1:reps) {
  xs <- cbind(xs,x)
  j <- sum(cumsum(p) < runif(1))+1; j
  x <- a[[j]] %*% x + b[[j]]
}
plot(xs[1,],xs[2,],pch=".")
