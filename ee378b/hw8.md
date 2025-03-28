---
title: "Charles Zheng EE 378b HW 8"
output:
  pdf_document: default
  html_document:
    mathjax: default
---

========================================================

# Charles Zheng EE 378b HW 8

# Setup


```r
library(pracma)
amat <- function(m, n) {
  am <- cbind(1 + cos( 2 * pi * (1:m) / m), sin( 2 * pi * (1:m) / m))
  bn <- cbind(3 * cos( 2 * pi * (1:n) / n), 3 * sin( 2 * pi * (1:n)/n))
  dm <- pdist2(am, bn)
  log(dm)
}

drineas <- function(a, c) {
  norms <- colSums(a ^ 2)
  ps <- norms/sum(norms)
  draws <- sample(dim(a)[2], c, replace= TRUE, prob=ps)
  t(t(a[, draws]) * (1/sqrt(c * ps[draws])))
}

plotter <- function(res, res_c, main) {
  ## match up SVecs with closest match
  mat <- t(res$u[, 1:5]) %*% res_c$u[, 1:5]
  z <- numeric(5)
  s <- numeric(5)
  for (i in 1:5) {
    z[i] <- order(-abs(mat[i, ]))[1]
    s[i] <- sign(mat[i, z[i]])
    mat[, z[i]] <- 0
  }
  plot(NA, NA, xlim = c(0, m),
       ylim = c(min(res$u[, 1:5]), max(res$u[, 1:5])),
       xlab = "m", ylab = "x")
  for (i in 1:5) {
    v0 <- res$u[, i]
    v <- s[i] * res_c$u[, z[i]]
    lines(v0, col = ccb[i], lwd = 1)    
    lines(v, col = cc[i], lwd = 3)  
    lines(rep(1:m, each = 2), rbind(pmin(v0, v), pmax(v0, v)), col = ccb[i], lwd = 1)
  }
  title(main)
}
cc <- rainbow(5, alpha = 1)
ccb <- rainbow(5, alpha = .5)
```

# 1


```r
n <- 2000; m <- 2000
a <- amat(m, n)
res <- svd(a)
plotter(res, res, "Exact Left SVs")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

# 2

Note: We have permuted and flipped the signs of approximate singular vectors for best match with exact singular vectors.


```r
set.seed(0)
plotter(res, svd(drineas(a, 10)), "c = 10")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
plotter(res, svd(drineas(a, 20)), "c = 20")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png) 

```r
plotter(res, svd(drineas(a, 30)), "c = 30")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-3.png) 

```r
plotter(res, svd(drineas(a, 60)), "c = 60")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-4.png) 

```r
plotter(res, svd(drineas(a, 120)), "c = 120")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-5.png) 

```r
plotter(res, svd(drineas(a, 240)), "c = 240")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-6.png) 

```r
plotter(res, svd(drineas(a, 480)), "c = 480")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-7.png) 

Seems we cannot recover the singular vectors exactly... why is that?
Check gap between singular vectors:

```r
plot(res$d[1:5], ylim = c(0, max(res$d)))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
res$d[1:5]
```

```
## [1] 2249.92039  354.27383  346.25312   67.15223   67.10196
```

Aha! It is difficult to recover the 2nd and 3rd singular vector exactly since their singular values are almost the same.  Same for the 4th and 5th singular vectors.
