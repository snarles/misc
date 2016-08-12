### implementing stability selection w/ randomized lasso

source("varsel/lasso_stability_source.R")
library(reginference)
data(HIV)
colnames(HIV)
# hist(HIV$NFV)
# 
# apply(HIV, 2, function(v) sum(v != 0))

x <- as.matrix(HIV[, -1])
y <- HIV$NFV

# res <- glmnet(as.matrix(HIV[, -1]), HIV$NFV, alpha = 1, standardize = FALSE)
# v <- coef(res, s = 0.1)
# which(v !=0) - 1

as <- ssel(x, y, s = 0.1, n.reps = 1000)
colSums(as)

pthres <- 0.8
## first selected
sel1 <- colnames(as)[colSums(as) > pthres * nrow(as)]
as2 <- as[, setdiff(colnames(as), sel1)]
sst <- list()
for (i in 1:ncol(as2)) {
  a3 <- as2
  a3[as2[, i]==1, ] <- 1
  sst[[i]] <- colnames(a3)[colSums(a3) > pthres * nrow(a3)]
}
names(sst) <- colnames(a3)
sst
