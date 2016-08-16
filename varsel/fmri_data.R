## Speed things up by using Kernel

####
## Load 3500 training data
####

library(magrittr)
library(pracma, warn.conflicts = FALSE)
library(MASS)
library(glmnet, warn.conflicts = FALSE)
library(prodlim)
source('utils/zattach.R')
source('transfer/source.R')
source('eb_ident/eigenprism.R')
source('eb_ident/bayes_reg.R')
source('eb_ident/source.R')




ddir <- '/home/snarles/stat312data/'
# ddir <- '/home/rstudio/stat312data/'

list.files(ddir)
# load(paste0(ddir, 'train_resp_all.RData'))
# dim(train_resp) # 1750 25915
load(paste0(ddir, 'roi.RData'))
dim(voxel.loc) # 25915 3
feat_attr <- read.table(paste0(ddir, "featAttr.csv"), header = TRUE, sep = ",")
dim(feat_attr)
load(paste0(ddir, 'photos_stimindex.RData'))
train_index <- read.table(paste0(ddir, "indexTrain.csv"), header = FALSE, sep = ",") %>% as.numeric
load(paste0(ddir, 'feature_train.RData'))
dim(feature_train) # 1750 10921
#train_resp_all <- read.csv('~/stat312data/allVoxTrain.csv', header = FALSE, sep = ",", na.strings = "NaN")
train_resp_all <- t(read.csv(gzfile('~/stat312data/allVoxTrain.csv.gz'),
                             header = FALSE, sep = ",", na.strings = c("NA", "NaN", "N"),
                             stringsAsFactors = FALSE))
dim(train_resp_all) #   3500 25927


converted_indices <- match(train_index, trainstim)
i_set1 <- match(1:1750, converted_indices)
i_set2 <- 3501 - match(1:1750, rev(converted_indices))
length(unique(c(i_set1, i_set2)))

train_resp_1 <- train_resp_all[i_set1, ]
train_resp_2 <- train_resp_all[i_set2, ]

dim(train_resp_1) # 1750 25927

# filt0 <- !is.na(colSums(train_resp))
filt1 <- !is.na(colSums(train_resp_1))
filt2 <- !is.na(colSums(train_resp_2))

# train_resp_f <- train_resp[, filt0]
train_resp_1f <- train_resp_1[, filt1]
train_resp_2f <- train_resp_2[, filt2]

# dim(train_resp_f) # 1750 22733
dim(train_resp_1f) # 1750 22733
dim(train_resp_2f) # 1750 22733

# filt_loc <- voxel.loc[filt0, ]
# roi2 <- sapply(roi, function(v) { voxel.loc[v, ] }, USE.NAMES = TRUE)
# roi3 <- sapply(roi2, function(v) {
#   row.match(as.data.frame(v), filt_loc) %>% {.[!is.na(.)]}
# }, USE.NAMES = TRUE)

#samp <- sample(22733, 100)
#image(cor(train_resp_f[, samp], train_resp_2f[, samp]))


# dim(Yall)
# Yvaris <- apply(Yall, 2, var)
# Ydiffs <- colSums((Yall[i_set1, ] - Yall[i_set2, ])^2)/3500
# plot(Yvaris, Ydiffs)
# Ysigs <- Yvaris - Ydiffs

## decide not to scale

####
##  Data Subsetting
####

Xall <- feature_train[converted_indices, ]
X1 <- Xall[i_set1, ]
X2 <- Xall[i_set2, ]

load("~/stat312data/wavpyr.RData")

rm(train_resp_1)
rm(train_resp_2)
gc()

X <- X1
Yvaris <- apply(rbind(train_resp_1f, train_resp_2f), 2, var)
Ydiffs <- colSums((train_resp_1f - train_resp_2f)^2)/3500
Ysigs <- Yvaris - Ydiffs
snrs <- Ydiffs/Ysigs
hist(snrs)
order(snrs, decreasing = TRUE)[1:50]

y <- train_resp_1f[, 14261]
var(y)

colnames(X) <- paste0("V", 1:ncol(X))

source("varsel/lasso_stability_source.R")
as <- ssel_no_scale(X, y, s = 0.01, n.reps = 2)
rowSums(as)


