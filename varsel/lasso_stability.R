### implementing stability selection w/ randomized lasso

library(glmnet)
library(reginference)


ssel <- function(x, y, s = 0.1, pw = 0.5, wkns = 0.9, n.reps = 100) {
  x <- as.matrix(x)
  y <- y - mean(y)
  as <- matrix(0, n.reps, ncol(x))
  colnames(as) <- colnames(x)
  n <- nrow(x)
  for ( i in 1:n.reps ) {
    inds <- sample(n, floor(n/2), replace = FALSE)
    yt <- y[inds]
    xt <- x[inds, ]
    vs <- apply(xt, 2, var)
    xt <- xt[, vs != 0]
    xt <- scale(xt)
    ws <- sample(c(wkns, 1), ncol(xt), replace = TRUE, prob = c(pw, 1-pw))
    xt <- t(t(xt)/ws)
    res <- glmnet(xt, yt, alpha = 1, standardize = FALSE)
    bt <- coef(res, s= s)
    nms <- rownames(as.matrix(bt))[which(bt > 0)]
    nms <- setdiff(nms, "(Intercept)")
    as[i, nms] <- 1
  }
  as
}

data(HIV)
colnames(HIV)
hist(HIV$NFV)

apply(HIV, 2, function(v) sum(v != 0))

x <- as.matrix(HIV[, -1])
y <- HIV$NFV

# res <- glmnet(as.matrix(HIV[, -1]), HIV$NFV, alpha = 1, standardize = FALSE)
# v <- coef(res, s = 0.1)
# which(v !=0) - 1

as <- ssel(x, y, s = 0.1)
colSums(as)
