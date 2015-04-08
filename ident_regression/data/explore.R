#############################################################
##                DATA PREPROCESSING                       ##
#############################################################

#savelist <- c()

isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

lf <- list.files("/home")
if ("snarles" %in% lf) (ddir <- "/home/snarles/stat312data")
if ("ubuntu" %in% lf) (ddir <- "/home/ubuntu/stat312data")
if ("rstudio" %in% lf) {
  (ddir <- "/home/rstudio/stat312data")
  setwd("/home/rstudio/misc/ident_regression/data/")
}
list.files(ddir)

library(Rcpp)
sourceCpp('pdist.cpp') # code from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/

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
omega_e <- isqrtm(sigma_e)

####
## REGRESSION
####

library(parallel)
library(glmnet)

lambdas <- 0:100/4000
nlambdas <- length(lambdas)

ntrials <- 200
misc_errors <- matrix(0, ntrials, nlambdas)
pr_errors <- matrix(0, ntrials, nlambdas)

library(class)

prfunc <- function(i) {
    res <- cv.glmnet(features_train, as.numeric(train_resp[i, ]),
                     standardize = FALSE, lambda = lambdas)
    pr <- predict(res, features_train, s=lambdas)
    list(pr = pr, cvm = res$cvm)
}
proc.time()
res <- list()
res[1:3] <- mclapply(1:3, prfunc, mc.cores = 3)
proc.time()

filt <- lapply(res, length)==0
filt
res[filt] <- mclapply(which(filt), prfunc, mc.cores = 30)
res2 <- mclapply(90:100, prfunc, mc.cores = 30)
res[90:100] <- res2

prfunc(3)

l <- prfunc(1)

cvm_error <- matrix(0, nlambdas, 100)
pr_error <- numeric(nlambdas)
misc_error <- numeric(nlambdas)
for (j in 1:100) {
    cvm_error[, j] <- res[[j]]$cvm
}

cvm_o <- apply(cvm_error, 1, mean)
lambdas[cvm_o == min(cvm_o)]
apply(cvm_error, 2, function(v) lambdas[v==min(v)])

lbda_i <- order(cvm_o)[1]

for (i in 1:nlambdas) {
    pvalid <- matrix(0, nte, 100)
    for (j in 1:100) {
        pvalid[, j] <- res[[j]]$pr[, i]
    }
    pr_error[i] <- sum((t(pvalid) - train_resp[, te_inds])^2)
}



####
## REGRESSION : USING TRAIN_RESP
####

lambdas <- 0:1000/40000
nlambdas <- length(lambdas)

ntrials <- 200
misc_errors <- matrix(0, ntrials, nlambdas)
pr_errors <- matrix(0, ntrials, nlambdas)

library(class)

proc.time()
for (ii in 1:ntrials) {
    tr_inds <- sample(1750, 1725, FALSE)
    te_inds <- setdiff(1:1750, tr_inds)
    nte <- length(te_inds)
    prfunc <- function(i) {
        res <- glmnet(features_train[tr_inds, ], as.numeric(train_resp[i, tr_inds]), standardize = FALSE)
        pr <- predict(res, features_train[te_inds, ], s=lambdas)
        pr
    }
    res <- mclapply(1:100, prfunc, mc.cores = 30)
                                        #res <- lapply(1:100, prfunc)
    pr_error <- numeric(nlambdas)
    misc_error <- numeric(nlambdas)
    for (i in 1:nlambdas) {
        pvalid <- matrix(0, nte, 100)
        for (j in 1:100) {
            pvalid[, j] <- res[[j]][, i]
        }
        pr_error[i] <- sum((t(pvalid) - train_resp[, te_inds])^2)
        te_cl <- knn(pvalid %*% omega_e, t(train_resp[, te_inds]) %*% omega_e, 1:nte, k=1)
        misc_error[i] <- misc_error[i] + sum(te_cl != 1:nte)
    }
    misc_errors[ii, ] <- misc_error
    pr_errors[ii, ] <- pr_error
    print(ii)
}
proc.time()

misc_error <- apply(misc_errors, 2, mean)
pr_error <- apply(pr_errors, 2, mean)

lambdas[order(misc_error)[1]]
lambdas[order(pr_error)[1]]

saveRDS(misc_errors, "misc_error.rds")
saveRDS(pr_errors, "pr_error.rds")

#plot(lambdas, misc_error)
#plot(lambdas, pr_error)
