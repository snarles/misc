library(pracma)


## empirically compute true null distribution
all_k_R2 <- function(X, mus, kmax) {
  p <- ncol(X)
  all_ans <- numeric()
  for (k in 1:kmax) {
    ss <- combn(1:p, k)
    ans <- apply(ss, 2, function(S) {
      res <- qr(X[, S])
      qr.Q(res)
      colSums((t(qr.Q(res)) %*% mus)^2)
    })
    colnames(ans) <- apply(ss, 2, function(S) {
      S <- S[S != 0]
      paste(S, collapse = "_")
    })
    all_ans <- cbind(all_ans, ans)
  }
  all_ans
}

fs_stat <- function(Vss) {
  ss_all <- lapply(colnames(Vss), function(v) {
    strsplit(v, "_")[[1]]
  })
  kmax <- max(sapply(ss_all, length))
  fsR2 <- apply(Vss, 1, function(v) {
    curr <- character()
    for (k in 1:kmax) {
      sets <- which(sapply(ss_all, function(w) 
        (length(setdiff(curr, w))==0) && (length(w)==k)))
      lala <- sets[which.max(v[sets])]
      curr <- ss_all[[lala]]
    }
    v[lala]
  })
  fsR2
}



n <- 1000
p <- 20
X <- randn(n, p)
X <- apply(X, 2, normalize)
k <- 3



# y <- normalize(rnorm(n))
# 
# Vs <- compute_true_Vs(X, y, ss)
# Vs2 <- compute_true_Vs2(X, y, ss)

n.its <- 10000
ys <- randn(n, n.its)
ys <- apply(ys, 2, normalize)
# Vs <- compute_true_Vs2(X, ys[, 1], ss)
Vss <- all_k_R2(X, ys, k)

sup_dist <- apply(Vss, 1, max)
hist(sup_dist)

fsR2 <- fs_stat(Vss)
# mean(sup_dist > 0.085)
# 
# ## probability of an intersection event
max(sup_dist)
max(fsR2)
thres <- 0.025
counts <- apply(Vss, 1, function(v) sum(v > thres))
barplot(table(counts)[-1])
approx_probs(Vss, thres, max.order = 5)
