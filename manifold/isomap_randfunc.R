library(rgl)
library(vegan)
library(RBGL)
library(graph)
library(Rgraphviz)
library(tree)

isomap_fast <- function(dist_, ndim = 10, epsilon = NA, k = NA) {
  dm <- as.matrix(dist_)
  rownames(dm) <- colnames(dm) <- paste0("v", 1:dim(dm)[1])
  diag(dm) <- NA
  if (is.na(epsilon)) {
    is.na(dm) <- apply(dm, 2, function(x) x > x[order(x, na.last = TRUE)[k]])
  } else {
    is.na(dm) <- apply(dm, 2, function(x) x >= epsilon)
  }
  dm[is.na(dm)] <- 0
  dm[dm == 0] <- t(dm)[dm == 0]
  dmg <- as(dm, "graphNEL")
  dm2 <- floyd.warshall.all.pairs.sp(dmg)
  res_mds <- cmdscale(dm2, k = ndim, eig = TRUE)
  res_mds
}

r <- 5 ## latent dimension
n <- 2000
z <- matrix(rnorm(n * r), n, r)

## parameters for random functions
nunits <- c(r, 3, 3)
sigma_e <- 0.1 ## sd of variance
p <- 100 ## number of x and y
p.x <- 80  ## number of x
fs <- lapply(1:p, function(i) random_ann(nunits))
xy0 <- do.call(cbind, lapply(fs, function(f) f(z)))
xy <- xy0 + sigma_e * matrix(rnorm(n * p), n, p)
x <- xy[, 1:p.x, drop = FALSE]
y <- xy[, (p.x+1):p, drop = FALSE]
summary(lm(y[, 1] ~ x))
summary(lm(y[, 1] ~ z))

res <- rpart(y[,1] ~ x)
summary(res)
