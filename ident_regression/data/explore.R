#############################################################
##                DATA PREPROCESSING                       ##
#############################################################

#savelist <- c()
alfa <- 0
isqrtm <- function(m) {
  res <- eigen(m)
  d <- res$values
  if (min(d) < -1e-5) warning("Negative eigenvalues in isqrtm")
  d[d < 0] <- 0
  d[d > 0] <- 1/sqrt(d[d > 0])
  v <- res$vectors
  return (v %*% diag(d) %*% t(v))
}

barsmat <- function(mat) {
  #mu <- apply(mat, 2, mean)
  #o <- order(mu)
  #mat <- mat[, o]
  mu <- apply(mat, 2, mean)
  sd <- apply(mat, 2, sd)/sqrt(dim(mat)[1])
  plot(1:dim(mat)[2], mu, ylim = c(min(mu-sd), max(mu+sd)))
  for (i in 1:dim(mat)[2]) {
    lines(c(i, i), mu[i] + sd[i]*c(-1, 1))
  }
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
## REGRESSION: Find the distance between image and ...
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
                     standardize = FALSE, lambda = lambdas, alpha = alfa)
    pr <- predict(res, features_train, s=lambdas)
    list(pr = pr, cvm = res$cvm)
}
proc.time()
res <- list()
res[1:25] <- mclapply(1:25, prfunc, mc.cores = 25)
res[25 + 1:25] <- mclapply(25 + 1:25, prfunc, mc.cores = 25)
res[50 + 1:25] <- mclapply(50 + 1:25, prfunc, mc.cores = 25)
res[75 + 1:25] <- mclapply(75 + 1:25, prfunc, mc.cores = 25)
proc.time()

sapply(res, length)

cvm_error <- matrix(0, nlambdas, 100)
for (j in 1:100) {
    cvm_error[, j] <- res[[j]]$cvm
}
cvm_o <- apply(cvm_error, 1, mean)
plot(lambdas, cvm_o)
plot(apply(cvm_error, 2, function(v) lambdas[v==min(v)]))
abline(lambdas[cvm_o == min(cvm_o)], 0)
lbda_i <- order(cvm_o)[1]
sel_lambda <- lambdas[lbda_i]
yhats <- matrix(0, 1750, 100)
for (j in 1:100) {
    yhats[, j] <- res[[j]]$pr[, lbda_i]
}
dim(yhats)
ys <- t(train_resp)
dim(ys) # 1750 100
w_ys <- ys %*% omega_e
w_yhats <- yhats %*% omega_e
class_dists0 <- sqrt(apply((w_ys - w_yhats)^2, 1, sum))
plot(class_dists0)

dim(w_ys)
w_ys_c <- t(t(w_ys) - apply(w_ys, 2, mean))
normz <- sqrt(apply(w_ys_c^2, 1, sum))
plot(normz, class_dists0)
pre_dists <- lm(class_dists0 ~ normz)$fitted
lm(class_dists0 ~ normz) # 0.8216 0.8616
plot(normz, class_dists0 - pre_dists)
misfitz <- class_dists0 - pre_dists

plot(ys[, 1], yhats[, 1])
i <- 8
i <- i + 1; scatter.smooth(ys[, i], yhats[, i], col = "red")

####
## REGRESSION: Stability of class distances under resampling
####

runif1 <- function(v) {set.seed(v); runif(1)}

metafunc1 <- function(v) {
    tr_inds <- sample(1750, 1000, FALSE)
    prfunc <- function(i) {
        res <- glmnet(features_train[tr_inds, ], as.numeric(train_resp[i, tr_inds]),
                         standardize = FALSE, alpha = alfa)
        pr <- predict(res, features_train, s=sel_lambda)
        list(pr = pr, cvm = res$cvm)
    }
    res <- lapply(1:100, prfunc)
    yhats <- matrix(0, 1750, 100)
    for (j in 1:100) {
        yhats[, j] <- res[[j]]$pr
    }
    w_yhats <- yhats %*% omega_e
    class_dists <- sqrt(apply((w_ys - w_yhats)^2, 1, sum))
    list(cd = class_dists, tr_inds = tr_inds)
}
dim(yhats)
ys <- t(train_resp)
dim(ys)
w_ys <- ys %*% omega_e
w_yhats <- yhats %*% omega_e
class_dists0 <- sqrt(apply((w_ys - w_yhats)^2, 1, sum))
plot(class_dists0)

####
## REGRESSION: Stability of class distances under resampling
####


proc.time()
temp <- metafunc1(4)
proc.time()
plot(temp$cd, class_dists0, pch = ".")
points(temp$cd[-temp$tr_inds], class_dists0[-temp$tr_inds], pch= '.', col= "red")

mclapply(1:25, runif1, mc.cores = 25)
res_dists <- mclapply(1:25, metafunc1, mc.cores = 25)

class_dists <- matrix(0, 25, 1750)
for (i in 1:25) class_dists[i, ] <- res_dists[[i]]$cd


mat <- class_dists
plot(apply(class_dists, 1, mean))
plot(apply(class_dists, 1, mean))

####
## REGRESSION : USING TRAIN_RESP
####

ntrials <- 200
misc_errors <- matrix(0, ntrials, 1750)
te_mask <- matrix(0, ntrials, 1750)
pr_errors <- matrix(0, ntrials, nlambdas)
ii <- 1

library(class)

proc.time()
for (ii in 1:ntrials) {
    tr_inds <- sample(1750, 1725, FALSE)
    te_inds <- setdiff(1:1750, tr_inds)
    nte <- length(te_inds)
    prfunc <- function(i) {
        res <- glmnet(features_train[tr_inds, ], as.numeric(train_resp[i, tr_inds]), 
                      standardize = FALSE, alpha = alfa)
        pr <- predict(res, features_train[te_inds, ], s=sel_lambda)
        pr
    }
    res <- mclapply(1:100, prfunc, mc.cores = 25)
    pvalid <- as.matrix(data.frame(res))
    te_cl <- knn(pvalid %*% omega_e, t(train_resp[, te_inds]) %*% omega_e, 1:nte, k=1)
    misc_error <- (te_cl != 1:nte)
    misc_errors[ii, te_inds] <- misc_error
    te_mask[ii, te_inds] <- 1
    print(ii)
}
proc.time()

plot(misc_errors[1, ])
plot(te_mask[2, ])

saveRDS(misc_errors, "me.rds")
saveRDS(te_mask, "te.rds")
sel_lambda # 0.0185

####
## load precomputed results
####

misc_errors <- readRDS("me.rds")
te_mask <- readRDS("te.rds")

ntrials <- dim(te_mask)[1]
flat_me <- c()
flat_inds <- c()

for (i in 1:ntrials) {
  indz <- which(te_mask[i, ] == 1)
  mez <- misc_errors[i, indz]
  flat_me <- c(flat_me, mez)
  flat_inds <- c(flat_inds, indz)
}

plot(misfitz[flat_inds], flat_me)
library(popbio)
logi.hist.plot(misfitz[flat_inds],flat_me,boxp=FALSE,type="hist",col="gray",
               xlab = "regression error", main = "Misclassification")
logi.hist.plot(normz[flat_inds],flat_me,boxp=FALSE,type="hist",col="gray",
               xlab = "normz", main = "Misclassification")
help(logi.hist.plot)

####
## do regression filtering out the misfits (stimuli)
####

reg_thres <- seq(0, 3, .5)

reg_filt <- function(seed) {
  set.seed(seed)
  nthres <- length(reg_thres)
  tr_inds <- sample(1750, 1725, FALSE)
  te_inds <- setdiff(1:1750, tr_inds)
  nte <- length(te_inds)
  results <- matrix(0, nthres, nte)
  for (i in 1:nthres) {
    reg_t <- reg_thres[i]
    ftr_inds <- tr_inds[misfitz[tr_inds] < reg_t]
    prfunc <- function(i) {
      res <- glmnet(features_train[ftr_inds, ], as.numeric(train_resp[i, ftr_inds]), 
                    standardize = FALSE, alpha = alfa)
      pr <- predict(res, features_train[te_inds, ], s=sel_lambda)
      pr
    }
    #res <- mclapply(1:100, prfunc, mc.cores = 25)    
    res <- lapply(1:100, prfunc)
    pvalid <- as.matrix(data.frame(res))
    te_cl <- knn(pvalid %*% omega_e, t(train_resp[, te_inds]) %*% omega_e, 1:nte, k=1)
    misc_error <- (te_cl != 1:nte)
    results[i, ] <- misc_error    
  }
  list(results = results, te_inds = te_inds)
}

res_filt <- list()
proc.time()
res_filt[1:25] <- mclapply(1:25, reg_filt, mc.cores = 25)
res_filt[25 + 1:25] <- mclapply(25 + 1:25, reg_filt, mc.cores = 25)
res_filt[50 + 1:25] <- mclapply(50 + 1:25, reg_filt, mc.cores = 25)
res_filt[75 + 1:25] <- mclapply(75 + 1:25, reg_filt, mc.cores = 25)
proc.time()

mus <- lapply(res_filt, function(v) apply(v$results, 1, mean))
mmus <- t(as.matrix(data.frame(mus)))
mumus <- apply(mmus, 2, mean)

barsmat(mmus)

plot(reg_thres, mumus, type = "o")

for (i in 1:25) {
  lines(reg_thres, mus[[i]], col = rainbow(25)[i])
}
