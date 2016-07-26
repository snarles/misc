library(MASS)
library(AlgDesign)

tab <- readRDS("../secret/klausner/database4.rds")
head(tab)
tab <- tab[complete.cases(tab), ]
head(tab)

inds_n <- which(unlist(lapply(lapply(tab, class), `[[`, 1)) == "numeric")
tab <- tab[, inds_n]
head(tab)
cbind(1:ncol(tab), colnames(tab))


X <- as.matrix(tab[, -13])
y <- tab[, 13]

head(X)

ss <- 1:3


(p <- ncol(X))

sss <- combn(1:p, 4)

gm <- 0.5
raw_log_post <- function(y, X, ss, return.rss = FALSE) {
  n <- length(y)
  yh <- X[, ss] %*% ginv(X[, ss]) %*% y
  rss <- sum((y - yh)^2)
  if (return.rss) {
    return(rss)
  }
  detr <- det(t(X[, ss]) %*% X[, ss], logarithm = TRUE)
  km <- length(ss)
  ans <- -km * log(gm) + (km/2) * log(pi) + 
    lgamma((n - km)/2) -1/2 * detr - (n - km)/2 * log(rss)
  ans
}

rss_s <- apply(sss, 2, function(v) raw_log_post(y, X, v, TRUE))

sss[, order(rss_s, decreasing = TRUE)[1:5]]
