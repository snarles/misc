---
title: Charles Zheng EE 378b HW 2
output:
  html_document:
    mathjax: default
---

Charles Zheng EE 378b HW 2
========================================================

Loading the data:


```r
tab <- read.csv("anon.csv", header = FALSE, row.names = 1)
colnames(tab) <- paste0("V", 1:63)
dim(tab)
```

```
## [1] 44 63
```

Define the following functions for plotting:

```r
cols <- rgb(runif(44) * .5, runif(44) * .5, runif(44) * .5)

plotfunc <- function(u) {
  plot(u, pch = '.')
  for (i in 1:44) {
    text(
      u[i, 1], u[i, 2], rownames(tab)[i],
      cex = .7, col = cols[i])
  }
}
```

Define a distance metric as follows:
$$
d(x, y) = 1 - \frac{\sum_i x_i y_i I_{x_i \neq NA}I_{y_i \neq NA}}{
\sqrt{\sum_i x_i^2 I_{x_i \neq NA}}
\sqrt{\sum_i y_i^2 I_{y_i \neq NA}}
}
$$

```r
cust_dist <- function(x, y) {
  inds <- which(!is.na(x) & !is.na(y))
  x2 <- x[inds]
  y2 <- y[inds]
  x2 <- x2/sqrt(sum(x2^2))
  y2 <- y2/sqrt(sum(y2^2))
  1 - sum(x2 * y2)
}
```

Before using spectral projection, take a look at linear projection.

# Linear projection

## Euclidean distance with softimpute


```r
library(softImpute)
x <- as.matrix(tab)
fits <- softImpute(x, type="svd", lambda = 2, rank.max = 2)
dim(fits$u) # 44 2
```

```
## [1] 44  2
```

```r
x2 <- complete(x, fits)
res <- svd(x2)
plotfunc(fits$u)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


## Custom distance with MDS


```r
dm <- matrix(0, 44, 44)
for (i in 1:44) {
  for (j in 1:44) {
    dm[i, j] <- cust_dist(x[i, ], x[j, ])
  }
}
fit <- cmdscale(dm)
plotfunc(fit)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

## Form the laplacian


```r
form_laplacian <- function(dm, sigma2) {
  amat00 <- exp(-dm/(2 * sigma2))
  ds <- apply(amat00, 1, sum)
  amat01 <- diag(1/sqrt(ds)) %*% amat00 %*% diag(1/sqrt(ds))
  amat <- diag(rep(1, dim(amat)[1])) - amat01
}
sigma2 <- 0.1
amat <- form_laplacian(dm, sigma2)
```

Function to find Laplacian coordinates.

```r
laplacian_coords <- function(amat, d = 2) {
  amat01 <- diag(rep(1, dim(amat)[1])) - amat
  res <- eigen(amat01)
  res$vectors[, 1:d]
}
coords <- laplacian_coords(amat)
plotfunc(coords)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

