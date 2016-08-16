library(glmnet)



# ssel <- function(x, y, s = 0.1, pw = 0.5, wkns = 0.9, n.reps = 100) {
#   x <- as.matrix(x)
#   y <- y - mean(y)
#   as <- matrix(0, n.reps, ncol(x))
#   colnames(as) <- colnames(x)
#   n <- nrow(x)
#   for ( i in 1:n.reps ) {
#     inds <- sample(n, floor(n/2), replace = FALSE)
#     yt <- y[inds]
#     xt <- x[inds, ]
#     vs <- apply(xt, 2, var)
#     xt <- xt[, vs != 0]
#     xt <- scale(xt)
#     ws <- sample(c(wkns, 1), ncol(xt), replace = TRUE, prob = c(pw, 1-pw))
#     xt <- t(t(xt)/ws)
#     res <- glmnet(xt, yt, alpha = 1, standardize = FALSE)
#     bt <- coef(res, s= s)
#     nms <- rownames(as.matrix(bt))[which(bt > 0)]
#     nms <- setdiff(nms, "(Intercept)")
#     as[i, nms] <- 1
#   }
#   as
# }

## without standardizing within
ssel <- function(x, y, s = 0.1, pw = 0.5, wkns = 0.9, n.reps = 100) {
  x <- as.matrix(x)
  x <- scale(x)
  y <- y - mean(y)
  as <- matrix(0, n.reps, ncol(x))
  colnames(as) <- colnames(x)
  n <- nrow(x)
  for ( i in 1:n.reps ) {
    inds <- sample(n, floor(n/2), replace = FALSE)
    yt <- y[inds]
    xt <- x[inds, ]
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


## without standardizing within, and getting k-sparse
ssel_no_scale <- function(x, y, s = 0.1, 
                          pw = 0.5, wkns = 0.9, 
                          k = 5, n.reps = 100) {
  x <- as.matrix(x)
  # x <- scale(x)
  y <- y - mean(y)
  as <- matrix(0, n.reps, ncol(x))
  colnames(as) <- colnames(x)
  n <- nrow(x)
  for ( i in 1:n.reps ) {
    inds <- sample(n, floor(n/2), replace = FALSE)
    yt <- y[inds]
    xt <- x[inds, ]
    ws <- sample(c(wkns, 1), ncol(xt), replace = TRUE, prob = c(pw, 1-pw))
    xt <- t(t(xt)/ws)
    res <- glmnet(xt, yt, alpha = 1, standardize = FALSE)
    bt <- get_lasso_k_sparse(res, k)
#    bt <- coef(res, s= s)
    nms <- rownames(as.matrix(bt))[which(bt > 0)]
    nms <- setdiff(nms, "(Intercept)")
    as[i, nms] <- 1
  }
  as
}


get_lasso_k_sparse <- function(res, k, s.min = 1e-5, s.max = 10) {
  s <- (s.min + s.max)/2
  lb <- s.min
  ub <- s.max
  for (i in 1:100) {
    bt <- coef(res, s)
    k.obs <- sum(bt > 0)
    if (k.obs > k) {
      lb <- s
      s <- (s + ub)/2
    }
    if (k.obs < k) {
      ub <- s
      s <- (s + lb)/2
    }
    #print(c(s, k.obs))
  }
  bt <- coef(res, s)
  bt
}

