#############################################################
##                          LOADING                        ##
#############################################################

library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/
ddir <- "/home/snarles/stat312data"
load(paste0(ddir, "/preproc_version1.RData"))
source("source1.R")
load(paste0(ddir, "/train_stim.RData"))
train_v1[,1] <- as.numeric(train_v1[, 1])
train_v1 <- as.matrix(train_v1)

#############################################################
##                          CCA                            ##
#############################################################

# get means by index
valid_means <- matrix(0, nvalid, nvox)
for (i in 1:nvalid) {
  valid_means[i, ] <- apply(valid_v1[, valid_index == i], 1, mean)
}

# apply canonical correlation analysis
library(PMA)
K_ <- 20
res <- CCA(valid_means, feature_valid_sdd, penaltyx = 0.1, penaltyz = 0.1, K = K_)
summary(res)
dim(res$u)
library(rgl)

## plot u
{
  plot3d(v1_locations)
  cols <- rainbow(K_)
  for (i in 1:K_) {
    points3d(v1_locations[res$u[, i] != 0, ], col=cols[i], size = K_ - i)
  }
}

## plot v
{
  old.par <- par()
  layout(matrix(1:16, 4, 4))
  par(mar = c(0,0,0,0))
  for (i in 1:16) {
    image(get_filter(res$v[, i], wav.pyr_filt))
  }
  par(old.par)
}

## apply the dimensionality reduction
valid_coords <- feature_valid_sdd %*% res$v
train_coords <- feature_train_sdd %*% res$v
dim(res$u)
dim(valid_v1)
valid_resp <- t(res$u) %*% valid_v1
train_resp <- t(res$u) %*% train_v1

class(train_v1)
which(sapply(train_v1, class) != "numeric")

dim(valid_coords)
dim(train_coords)

## distance matrix of 1750 x 1750 images based on grand mean
mdists <- fastPdist2(train_coords, train_coords)

#############################################################
##                  Look at clusters                       ##
#############################################################

## do K-means
res_k <- kmeans(train_coords, centers = 100)
length(res_k$cluster)
table(res_k$cluster)
## write the clusters
library(png)
ind <- 1
for (ind in 1:max(res_k$cluster)) {
  fname = paste("train_clust/",ind,".png",sep="")
  nclust <- sum(res_k$cluster == ind)
  nrows <- ceiling(nclust / 5)
  bigimg <- matrix(0, 128 * nrows, 128 * 5)
  xloc <- 0
  yloc <- 0
  inds <- which(res_k$cluster == ind)
  if (length(inds) > 25) inds <- inds[1:25]
  for (ind in inds) {
    img <- train_stim[ind,]+.5
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

#############################################################
##        Distance matrix between coordinates              ##
#############################################################

valid_dm <- fastPdist2(valid_coords, valid_coords)
train_dm <- fastPdist2(train_coords, train_coords)


#############################################################
##        Gaussian classification for validation           ##
#############################################################

dim(valid_resp)
dim(train_resp)

classes <- sample(nvalid, 20)
res <- do_gauss_class(valid_resp, valid_index, classes, 11)
res$err_rate



