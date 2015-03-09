#############################################################
##                DATA PREPROCESSING                       ##
#############################################################

savelist <- c()

library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/

ddir <- "/home/snarles/stat312data"
list.files(ddir)

## get indices of V1 from larger matrix
load(paste0(ddir, "/all_voxel_locations.RData"))
dim(voxel.loc) # 25915 3
savelist <- c(savelist, "voxel.loc")
load(paste0(ddir, "/v1_locations.RData"))
dim(v1_locations) # 1331 3
library(prodlim)
v1_inds <- row.match(data.frame(v1_locations), data.frame(voxel.loc))

## extract V1 voxels in training data
temp <- read.csv(paste0(ddir, "/allVoxTrain.csv"), header = FALSE,
                 stringsAsFactors = FALSE)
train_v1 <- temp[v1_inds, ]
train_v1[,1] <- as.numeric(train_v1[, 1])
save(train_v1, file = paste0(ddir, "train_v1.RData"))
load(paste0(ddir, "/train_v1.RData"))
load(paste0(ddir, "/valid_index.RData"))
savelist <- c(savelist, "valid_index")
train_index <- read.csv(paste0(ddir, "/indexTrain.csv"), header = FALSE)
train_index <- as.vector(train_index)
savelist <- c(savelist, "train_index")
load(paste0(ddir, "/feature_valid.RData"))
load(paste0(ddir, "/feature_train.RData"))
dim(feature_train) # 1750 10921
dim(feature_valid) # 120 10921
#load(paste0(ddir, "/train_stim.RData"))
#load(paste0(ddir, "/valid_stim.RData"))
load(paste0(ddir, "/valid_v1.RData"))
dim(valid_v1) # 1331 1560
dim(train_v1) # 1331 3500

ntime <- 1560
nvalid <- 120
ntrain <- 1750
savelist <- c(savelist, "ntime", "nvalid", "ntrain")


## Remove missing values
v1_filt1 <- (apply(valid_v1, 1, function(x) {sum(is.na(x))}) == 0)
v1_filt2 <- (apply(train_v1, 1, function(x) {sum(is.na(x))}) == 0)
v1_filt <- v1_filt1 & v1_filt2
v1_locations <- v1_locations[v1_filt, ]
valid_v1 <- valid_v1[v1_filt, ]
train_v1 <- train_v1[v1_filt, ]
(nvox <- sum(v1_filt)) #1294
savelist <- c(savelist, "nvox")
savelist <- c(savelist, "valid_v1")
savelist <- c(savelist, "train_v1")
savelist <- c(savelist, "v1_locations")

## apply cutoff for features
load(paste0(ddir, "/wavpyr.RData"))
stddevs <- apply(feature_valid, 2, sd)
gwp_filt_inds <- which(stddevs > 0.1)
feature_valid_filt <- feature_valid[, gwp_filt_inds]
feature_train_filt <- feature_train[, gwp_filt_inds]
wav.pyr_filt <- wav.pyr[, gwp_filt_inds]
save(feature_valid_filt, feature_train_filt, wav.pyr_filt, gwp_filt_inds,
     file = paste0(ddir, '/data_feature_filt.RData'))
#load(paste0(ddir, '/data_feature_filt.RData'))
dim(wav.pyr_filt)
dim(feature_train_filt)
dim(feature_valid_filt)
(nfeatures <- dim(feature_valid_filt)[2])
savelist <- c(savelist, "nfeatures")

## standardize features using validation data
mus <- apply(feature_valid_filt, 2, mean)
sds <- apply(feature_valid_filt, 2, sd)
dim(feature_valid_filt) # 1750 2248
dim(feature_train_filt) # 1750 2248
feature_train_sdd <- t((t(feature_train_filt) - mus)/sds)
dim(feature_train_sdd)
feature_valid_sdd <- t((t(feature_valid_filt) - mus)/sds)
dim(feature_valid_sdd)
savelist <- c(savelist, "wav.pyr_filt")
savelist <- c(savelist, "feature_train_sdd")
savelist <- c(savelist, "feature_valid_sdd")

## write to file
save(list = savelist, file = paste0(ddir, "/preproc_version1.RData"))
