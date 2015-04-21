library(rgl)
library(vegan)
library(RBGL)
library(graph)
library(Rgraphviz)

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

tab <- readRDS('survey_Aug14.rds')

res <- isomap_fast(dist(tab), k = 20)
scores(res)


