library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/

ddir <- "/home/snarles/stat312data"
list.files(ddir)
#load(paste0(ddir, "/all_voxel_locations.RData"))
load(paste0(ddir, "/valid_index.RData"))
#load(paste0(ddir, "/feature_train.RData"))
#train_resp_all <- read.csv(paste0(ddir, "/train_resp_all.csv"), header = FALSE)
#valid_resp_all <- read.csv(paste0(ddir, "/valid_resp_all.csv"), header = FALSE)
#load(paste0(ddir, "/feature_valid.RData"))
#load(paste0(ddir, "/train_stim.RData"))
load(paste0(ddir, "/valid_stim.RData"))
load(paste0(ddir, "/valid_v1.RData"))
load(paste0(ddir, "/v1_locations.RData"))
dim(valid_v1)
nvox <- 1331
ntime <- 1560

# apply rank transform
valid_v1_rank <- apply(valid_v1, 2, rank)/nvox
dim(valid_v1_rank)
apply(valid_v1_rank, 2, min)

# select 20 images, try to learn subspace
filt_imgs <- sample(120, 20)
filt_valid_index <- valid_index[valid_index %in% filt_imgs]
filt_v1_rank <- valid_v1_rank[, valid_index %in% filt_imgs]
dim(filt_v1_rank)
svd_res <- svd(filt_v1_rank) # svd is u %*% diag(d) %*% t(v)
temp <- eval(quote(u %*% diag(d) %*% t(v)), envir = svd_res)
temp[1:10]
filt_v1_rank[1:10]

comp1 <- matrix(filt_valid_index[row(base_dists)],
                nrow = length(filt_valid_index))

comp2 <- matrix(filt_valid_index[col(base_dists)],
                nrow = length(filt_valid_index))

compd <- row(base_dists)==col(base_dists)

twincomp <- matrix(
  filt_valid_index[row(base_dists)] == filt_valid_index[col(base_dists)],
  nrow = length(filt_valid_index)
)
othercomp <- !twincomp
diag(twincomp) <- !diag(twincomp)

# baseline distance
base_dists <- fastPdist2(t(filt_v1_rank), t(filt_v1_rank))
base_twins <- base_dists[twincomp]
summary(base_twins)
base_other <- base_dists[othercomp]
summary(base_other)

base_probs <- numeric(length(unique(filt_valid_index)))

for (i in 1:length(unique(filt_valid_index))) {
  img_ind <- filt_valid_index[i]
  twins <- base_dists[comp1 == img_ind & comp2 == img_ind & !compd]
  others <- base_dists[comp1 == img_ind & comp2 != img_ind]
  base_probs[i] <- sum(sample(others, 1000, TRUE) > 
                         sample(twins, 1000, TRUE))/1000
}

base_probs
