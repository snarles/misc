#############################################################
##                DATA PREPROCESSING                       ##
#############################################################

savelist <- c()

library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/

#ddir <- "/home/snarles/stat312data"
ddir <- "/home/ubuntu/stat312data"
list.files(ddir)


## get indices of V1 from larger matrix
load(paste0(ddir, "/forCharlesSNR.Rdata"))
best_v <- order(-snr_ests)[1:100]
load(paste0(ddir, "/all_voxel_locations.RData"))
dim(voxel.loc) # 25915 3
load(paste0(ddir, "/v1_locations.RData"))
v1_locations <- v1_locations[best_v, ]
dim(v1_locations) # 100 3
library(prodlim)
v1_inds <- row.match(data.frame(v1_locations), data.frame(voxel.loc))



## extract V1 voxels in training data
temp <- read.csv(paste0(ddir, "/allVoxTrain.csv"), header = FALSE,
                 stringsAsFactors = FALSE)
train_v1 <- temp[v1_inds, ]
train_v1[,1] <- as.numeric(train_v1[, 1])

load(paste0(ddir, "/valid_index.RData"))

train_index <- read.csv(paste0(ddir, "/indexTrain.csv"), header = FALSE)
train_index <- as.numeric(train_index)

load(paste0(ddir, "/train_resp.RData"))

load(paste0(ddir, "/feature_valid.RData"))
load(paste0(ddir, "/feature_train.RData"))
dim(feature_train) # 1750 10921
dim(feature_valid) # 120 10921

load(paste0(ddir, "/valid_v1.RData"))
valid_v1 <- valid_v1[best_v, ]
dim(train_v1)
dim(valid_v1)


train_resp <- read.csv(paste0(ddir, "/train_resp_all.csv"), header = FALSE)
dim(train_resp) #25915 1750
train_resp <- train_resp[v1_inds, ]

feat_attr <- read.csv(paste0(ddir, "/featAttr.csv"), header = TRUE,
                 stringsAsFactors = FALSE)
feat_lv <- feat_attr[2, ]

#####
## PROCESSING IMAGE FEATURES
#####

inds_train <- 1:1750
inds_valid <- 1750 + 1:120
features_all <- rbind(feature_train, feature_valid)
vars <- apply(features_all, 2, var)
lvars <- log(apply(features_all, 2, var))
plot(sort(lvars), type ="l")
var_filt <- (lvars > -10)
sum(var_filt)
dim(feat_attr)

comp_var <- sapply(
  1:4,
  function(i) {
    temp_filt <- var_filt & (feat_lv == i)
    median(vars[temp_filt])
  })
comp_var

for (i in 1:4) {
  features_all[, feat_lv == i] <-
    features_all[, feat_lv == i]/sqrt(comp_var[i])
}

features_all <- features_all[, var_filt]
features_train <- features_all[inds_train, ]
features_valid <- features_all[inds_valid, ]
feat_attr <- feat_attr[, var_filt]

dim(features_train)
train_index
#x_train <- features_train[train_index, ]
dim(train_v1)
length(train_index)
length(unique(train_index))
max(train_index)
max(valid_index)

####
## COVARIANCE OF ERROR
####

dim(features_train)
dim(train_v1)
train_index[1:10]
dm_train_v1 <- train_v1
i <- train_index[1]
for (i in unique(train_index)) {
    filt <- train_index == i
    dm_train_v1[, filt] <-
      t(apply(train_v1[, filt], 1, function(v) v - mean(v)))
}
dim(dm_train_v1)
sigma_e <-cov(t(dm_train_v1))
eye <- mean(diag(sigma_e)) * diag(rep(1, 100))
sigma_e <- 0.5 * sigma_e + 0.5 * eye
omega_e <- solve(sigma_e)

####
## REGRESSION
####

library(parallel)
library(glmnet)
cl <- makeCluster(5)

dim(features_train)
dim(train_resp)

prfunc <- function(i) {
    as.numeric(train_resp[1,])
    res <- glmnet(features_train, as.numeric(train_resp[1, ]))
    pr <- predict(res, features_valid, s=0:10/10)
    pr
}

res <- lapply(1:10, prfunc)
