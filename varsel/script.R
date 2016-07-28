library(MASS)
library(AlgDesign)

f2 <- function(x, y=0) sum((x - y)^2)


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
  Xm <- cbind(1, X[, ss])
  yh <- Xm %*% ginv(Xm) %*% y
  rss <- sum((y - yh)^2)
  if (return.rss) {
    return(rss)
  }
  detr <- determinant(t(X[, ss]) %*% X[, ss], logarithm = TRUE)$modulus
  km <- length(ss)
  ans <- -km * log(gm) + (km/2) * log(pi) + 
    lgamma((n - km)/2) -1/2 * detr - (n - km)/2 * log(rss)
  ans
}

post_normalization <- function(pv, log = TRUE) {
  if (log) {
    pv <- pv - max(pv)
    pv <- exp(pv)
  }
  pv/sum(pv)
}

rss_s <- apply(sss, 2, function(v) raw_log_post(y, X, v, TRUE))

ssso <- sss[, order(rss_s)]
rsso <- rss_s[order(rss_s)]
sss2 <- matrix(colnames(X)[sss], nrow = nrow(sss))
sss2[, order(rss_s)[1:5]]


rlps <- apply(sss, 2, function(v) raw_log_post(y, X, v))
rlps2 <- post_normalization(rlps)
raw_log_post(y, X, 1:4, FALSE)

rlps[order(-rlps)][1:5]
sss2[, order(-rlps)[1:5]]
cprob <- cumsum(rlps2[order(-rlps)])
cprob[1:5]
