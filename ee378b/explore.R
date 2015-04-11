####
## Charles Zheng
## EE 378b HW 2
####


tab <- read.csv("anon.csv", header = FALSE, row.names = 1)
head(tab)
dim(tab) # 44 63
colnames(tab) <- paste0("V", 1:63)
head(tab)

## functions for plotting

plotfunc <- function(u) {
  plot(u, pch = '.')
  for (i in 1:44) {
    text(u[i, 1], u[i, 2], rownames(tab)[i], cex = .7, col = rgb(runif(1) * .5,
                                                                  runif(1) * .5,
                                                                  runif(1) * .5))
  }  
}

## use matrix completion

library(softImpute)
x <- as.matrix(tab)
fits <- softImpute(x, trace=TRUE, type="svd", lambda = 2, rank.max = 2)
dim(fits$u) # 44 2
plotfunc(fits$u)
x2 <- complete(x, fits)
res <- svd(x2)

## define distance

cust_dist <- function(x, y) {
  inds <- which(!is.na(x) & !is.na(y))
  x2 <- x[inds]
  y2 <- y[inds]
  x2 <- x2/sqrt(sum(x2^2))
  y2 <- y2/sqrt(sum(y2^2))
  1 - sum(x2 * y2)
}

x["information and coding theory", ]

cust_dist(x["information and coding theory",], x["statistical inference graphical models", ])
cust_dist(x["information and coding theory",], x["quantum information", ])
cust_dist(x["information and coding theory",], x["biomedical devices", ])

dm <- matrix(0, 44, 44)
for (i in 1:44) {
  for (j in 1:44) {
    dm[i, j] <- cust_dist(x[i, ], x[j, ])
  }
}

fit <- cmdscale(dm)
plotfunc(fit)

sigma2 <- 0.1
amat <- exp(-dm/(2 * sigma2))
