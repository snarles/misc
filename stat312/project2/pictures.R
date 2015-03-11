png('valid_weight1.png')
plot(solve_unif(valid_dm, 1), ylim = c(0, 0.2))
title("h = 1")
dev.off()

png('valid_weight2.png')
plot(solve_unif(valid_dm, 2), ylim = c(0, 0.2))
title("h = 2")
dev.off()

png('valid_weight5.png')
plot(solve_unif(valid_dm, 5), ylim = c(0, 0.2))
title("h = 5")
dev.off()

png('valid_weight10.png')
plot(solve_unif(valid_dm, 10), ylim = c(0, 0.2))
title("h = 10")
dev.off()

png('valid_weight20.png')
w <- solve_unif(valid_dm, 20)
max(w)
plot(w, ylim = c(0, 0.2))
title("h = 20")
abline(0.02, 0)
dev.off()


inds <- which(w > 0.02)
length(inds)

library(png)
nrows <- ceiling(length(inds) / 5)
bigimg <- matrix(0, 128 * nrows, 128 * 5)
xloc <- 0
yloc <- 0
if (length(inds) > 25) inds <- inds[1:25]
for (ind in inds) {
  img <- valid_stim[ind,]+.5
  img[img > 1]=1
  img[img < 0]=0
  img <- matrix(img,128,128)
  bigimg[128*yloc + 1:128, 128*xloc + 1:128] <- img
  xloc <- xloc + 1
  if (xloc > 4) {
    yloc <- yloc + 1
    xloc <- 0
  }
}
writePNG(bigimg,'valid_20.png')


## measure diversity

temp <- exp(-valid_dm^2)
-t(rep(1/nvalid, nvalid)) %*% temp %*% rep(1/nvalid, nvalid)
# -0.0083

temp <- exp(-train_dm^2)
-t(rep(1/ntrain, ntrain)) %*% temp %*% rep(1/ntrain, ntrain)
# -0.00057


temp <- exp(-(valid_dm/5)^2)
-t(rep(1/nvalid, nvalid)) %*% temp %*% rep(1/nvalid, nvalid)
# -0.01

temp <- exp(-(train_dm/5)^2)
-t(rep(1/ntrain, ntrain)) %*% temp %*% rep(1/ntrain, ntrain)
# -0.003
