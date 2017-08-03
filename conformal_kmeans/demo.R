## kmeans with conformal clustering

## generate true data
set.seed(1)
library(pracma)
true_k <- 5
p <- 2
sigma <- 0.1
ntr <- 100
nte <- 50
mus <- randn(true_k, p)
ztr <- sample(true_k, ntr, replace = TRUE)
zte <- sample(true_k, nte, replace = TRUE)
ytr <- mus[ztr, ] + sigma * randn(ntr, p)
yte <- mus[zte, ] + sigma * randn(nte, p)
plot(ytr)

## kmeans on training
k <- 5
res <- kmeans(ytr, k)
centers <- res$centers

## conformal scores on test data
dd <- pdist2(yte, centers)
mindist <- apply(dd, 1, min)

## new conformal score
alph <- 0.05
(radius <- sort(mindist)[ceiling((1-alph) * (nte + 1))])

## plot the confidence interval
library(plotrix)
plot(ytr, col = "black", asp =1, pch = ".")
for (i in 1:k) {
  draw.circle(centers[i, 1], centers[i, 2], radius, col = rgb(0,0,0,0.3))
}


for (k in 1:9) {
  res <- kmeans(ytr, k)
  centers <- res$centers
  dd <- pdist2(yte, centers)
  mindist <- apply(dd, 1, min)
  (radius <- sort(mindist)[ceiling((1-alph) * (nte + 1))])
  
  pdf(paste0("conformal_kmeans/k", k, ".pdf"), width = 4, height = 4)
  plot(ytr, col = "black", asp =1, pch = ".", main = paste("k =", k))
  for (i in 1:k) {
    draw.circle(centers[i, 1], centers[i, 2], radius, col = rgb(0,0,0,0.3))
  }
  dev.off()
  
}
