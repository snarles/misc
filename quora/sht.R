####
##  Simple hypothesis tests and classification
####

set.seed(0)
xs <- 0:100/100
gg <- .5 * dbeta(xs, 40, 10) + .5 * dbeta(xs, 50, 70)
ff <- dbeta(xs, 30, 20)
par("mgp") # [1] 3 1 0
par("mar") #5.1 4.1 4.1 2.1
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mgp = c(1, 1, 0))
plot(xs, ff, type = 'l', lwd = 2, xlab = "x", ylab = "density",
     axes = FALSE, frame.plot = TRUE, cex.lab = 2)
lines(xs, gg, lwd = 2, lty = 2, col = "blue")
text(0.288, 2.9, "g", col = "blue", cex = 3)
text(0.6, 3.9, "f", cex = 3)

gsamp <- c(rbeta(50, 40, 10), rbeta(50, 50, 70))
fsamp <- rbeta(100, 30, 20)

par(mgp = c(3, 1, 0))
hist(c(gsamp, fsamp), breaks = 20, main = "Data", xlab = "x")

par(mgp = c(1, 1, 0))
plot(0:1, 0:1, col = "white", xlab = "x", ylab = "",  axes = FALSE, frame.plot = TRUE, cex.lab = 2)
points(gsamp, rep(0.7, 100))
points(fsamp, rep(0.3, 100))
text(0.1, 0.3, "F", cex = 2)
text(0.1, 0.7, "G", cex = 2)

lr <- log(gg/ff)
olr <- rank(-lr)/length(lr)
plot(xs, log(gg/ff))
points(xs[which(olr < .3)], 0 * which(olr < .3), pch = "+")


pts2intervals <- function(v, xs = 1:length(v)) {
  bks <- c(0, which(v[-1] - v[-length(v)] > 1), length(v))
  ans <- as.list(numeric(length(bks) - 1))
  for (i in 1:(length(bks) - 1)) {
    ans[[i]] <- xs[v[c(bks[i] + 1, bks[i+1])]]
  }
  ans
}
ints <- pts2intervals(which(olr < .76), xs)


par(mgp = c(1, 1, 0))
plot(0:1, 0:1, col = "white", xlab = "x", ylab = "",  axes = FALSE, frame.plot = TRUE, cex.lab = 2)
points(gsamp, rep(0.7, 100))
points(fsamp, rep(0.3, 100))
text(0.1, 0.3, "F", cex = 2)
text(0.1, 0.7, "G", cex = 2)
for(lu in ints) polygon(lu[c(1, 2, 2, 1)], c(0, 0, 1, 1), border = rgb(.2, .2, .2),
                        col = rgb(.3, .3, .3, .3))

ints


nrejected <- function(samp, ints) {
  s <- 0
  for (lu in ints) s <- s + sum(samp > lu[1] & samp < lu[2])
  s
}
nrejected(fsamp, ints)
nrejected(gsamp, ints)

nrejectedfunc <- function(prop) {
  ints <- pts2intervals(which(olr < prop), xs)
  res <- c(nrejected(fsamp, ints)/length(fsamp), nrejected(gsamp, ints)/length(gsamp))
  res
}

lala <- do.call(cbind, lapply(0:100/100, nrejectedfunc))
lala

par(mgp = c(3, 1, 0))
plot(t(lala[, 13:91]), xlab = "FPR", ylab = "TPR", ylim = 0:1, xlim = 0:1, type = "o")
abline(0, 1, lty = 2)
