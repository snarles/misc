library(pracma)


## mix gaussian

mc.reps <- 1e6
n <- 10
samps <- randn(mc.reps, n)
zs <- floor(rand(mc.reps, n) * 3 - 1)
xs <- samps + zs

## get samp var

xbars <- rowMeans(xs)
s2hats <- rowMeans((xs - xbars)^2) * n/(n-1)

## moments

(mu <- mean(xs))
(mu2 <- mean(xs^2))
(mu4 <- mean(xs^4))

## compute var of s2

c(mean(s2hats^2),
  1/(n-1)^2 * (
    n^2 * (2 * mean(xbars^2 * x1^2) - 4 * mean(xbars^3 * x1) + mean(xbars^4)) +
      n * (mean(x1^4) - 4 * mean(xbars * x1^3) + 4 * mean(xbars^2 * x1^2)) + 
      n*(n-1) * (mean(x1^2 * x2^2) - 4 * mean(xbars * x1 * x2^2) + 4 * mean(xbars^2 * x1 * x2))
    ))

c(var(s2hats), 
  1/(n-1)^2 * (
    n^2 * (2 * mean(xbars^2 * x1^2) - 4 * mean(xbars^3 * x1) + mean(xbars^4)) +
      n * (mean(x1^4) - 4 * mean(xbars * x1^3) + 4 * mean(xbars^2 * x1^2)) + 
      n*(n-1) * (mean(x1^2 * x2^2) - 4 * mean(xbars * x1 * x2^2) + 4 * mean(xbars^2 * x1 * x2))
  ) - mu2^2)

## intermediate calculations

x1 <- xs[, 1]; x2 <- xs[, 2]

c(mean(xbars^4),
  1/n^4 * (n * mu4 + 3 * n*(n-1) * mu2^2))

c(mean(xbars^3 * x1),
  1/n^3 * (mu4 + 3 * (n-1) * mu2^2))

c(mean(xbars^2 * x1^2),
  1/n^2 * ((n-1) * mu2^2 + mu4))

c(mean(xbars^2 * x1 * x2),
  1/n^2 * (2 * mu2^2))

c(mean(xbars * x1^3),
  1/n * mu4)

c(mean(xbars *x1 * x2^2),
  1/n * (mu2^2))

c(mean(x1^2 * x2^2),
  mu2^2)

