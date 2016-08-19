library(MASS)
library(corrplot)
source("varsel/lasso_stability_source.R")
source("varsel/generate_spatial_cov.R")

draw_pop_ssel <- function(Sigma, bt, n, sigma2, s, pw, wkns, n.reps = 100) {
  ans <- numeric()
  for (i in 1:n.reps) {
    Xy <- gen_Xy(Sigma, bt, n, sigma2)
    x <- Xy$X; y <- Xy$y
    as0 <- ssel0(x, y, s, pw, wkns)
    ans <- rbind(ans, as0)
  }
  ans
}

draw_pop_fsel <- function(Sigma, bt, n, sigma2, k, n.reps = 100) {
  ans <- numeric()
  for (i in 1:n.reps) {
    Xy <- gen_Xy(Sigma, bt, n, sigma2)
    x <- Xy$X; y <- Xy$y
    as0 <- fwdsel(x, y, k)
    ans <- rbind(ans, as0)
  }
  ans
}

p <- 40
d <- 2
h <- 0.1
Sigma <- gen_Sigma(p, d, h)
sigma2 <- 7

inds <- 1:5
bt <- rep(0, p)
bt[inds] <- 1

## generation params
n <- 100

## selection params
# s <- 1
# pw <- 0.5
# wkns <- 0.9
# 
# as_pop <- draw_pop_ssel(Sigma, bt, n, sigma2, s, pw, wkns, 100)
# colnames(as_pop) <- paste0("V", 0:p)
# as_pop <- as_pop[, -1]
# props <- colSums(as_pop)
# as_pop <- as_pop[, order(props, decreasing = TRUE)]
# image(t(as_pop))





####
##  forward stepwise
####

k <- 4
n.reps <- 1000
## pop version
as_pop <- draw_pop_fsel(Sigma, bt, n, sigma2, k, n.reps)
# colnames(as_pop) <- paste0("V", 1:p)
# props <- colSums(as_pop)
# ksp <- rowSums(as_pop)
# as_pop <- as_pop[order(ksp), order(props, decreasing = TRUE)]

## boot version
Xy <- gen_Xy(Sigma, bt, 2 * n, sigma2)
x <- Xy$X; y <- Xy$y
as1 <- bootfwd(x, y, k, n.reps)

image(t(as_pop))
image(t(as1))

corrplot(cor(as_pop))
corrplot(cor(as1))

