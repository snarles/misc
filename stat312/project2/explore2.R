#############################################################
##                          LOADING                        ##
#############################################################

library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/
ddir <- "/home/snarles/stat312data"
load(paste0(ddir, "/preproc_version1.RData"))

#############################################################
##                          CCA                            ##
#############################################################


# get means by index
valid_means <- matrix(0, nvalid, nvox)
for (i in 1:nvalid) {
  valid_means[i, ] <- apply(valid_v1[, valid_index == i], 1, mean)
}
valid_means[1, ]


# apply canonical correlation analysis
library(PMA)
K_ <- 20
res <- CCA(valid_means, feature_valid_filt, penaltyx = 0.1, penaltyz = 0.1, K = K_)
summary(res)
dim(res$u)
library(rgl)

# plot u[,1]
plot3d(v1_locations)
cols <- rainbow(K_)
for (i in 1:K_) {
  points3d(v1_locations[res$u[, i] != 0, ], col=cols[i], size = K_ - i)
}

# apply the dimensionality reduction
dim(res$u)
dim(valid_v1)
valid_reduced <- t(res$u) %*% valid_v1
dim(valid_reduced)
mu_valid_reduced <- t(res$u) %*% t(valid_means)
grand_mu_reduced <- apply(mu_valid_reduced, 1, mean)
length(grand_mu_reduced)

# estimate covariances
covs_valid_reduced <- array(0, dim = c(K_, K_, nvalid))
for (i in 1:nvalid) {
  covs_valid_reduced[, , i] <- cov(t(valid_reduced[, valid_index==i]))
}
grand_cov_reduced <- apply(covs_valid_reduced, c(1,2), mean)
dim(grand_cov_reduced)

library(corrplot)
corrplot(grand_cov_reduced, is.corr = FALSE)
corrplot(covs_valid_reduced[, , 1], is.corr = FALSE)
corrplot(covs_valid_reduced[, , 2], is.corr = FALSE)

# distance matrix of 120 x 120 images based on grand mean
mu_centered <- mu_valid_reduced - grand_mu_reduced
mdists <- matrix(0, nvalid, nvalid)
for (i in 1:nvalid) {
  mdists[i, ] <- mahalanobis(t(mu_valid_reduced), mu_valid_reduced[, i], grand_cov_reduced)
}
corrplot(mdists, is.corr = FALSE)

# do K-means based on whitened coordinates
# inverse square root funtion
isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}
whiten_mat <- isqrtm(grand_cov_reduced)
mu_whitened <- whiten_mat %*% mu_centered
dim(mu_whitened)
res_k <- kmeans(t(mu_whitened), centers = 20)
res_k$cluster
table(res_k$cluster)
# write the clusters
library(png)
for (ind in 1:max(res_k$cluster)) {
  fname = paste("cluster",ind,".png",sep="")
  nclust <- sum(res_k$cluster == ind)
  nrows <- ceiling(nclust / 5)
  bigimg <- matrix(0, 128 * nrows, 128 * 5)
  xloc <- 0
  yloc <- 0
  for (ind in which(res_k$cluster == ind)) {
    img <- valid_stim[ind,]+.5
    img[img > 1]=1
    img[img < 0]=0
    img <- matrix(img,128,128)
    bigimg[128*yloc + 1:128, 128*xloc + 1:128] <- img
    xloc <- xloc + 1
    if (xloc > 4) {
      yloc <- yloc + 1
      xloc <- 0
    }
  }
  writePNG(bigimg,fname)
}
