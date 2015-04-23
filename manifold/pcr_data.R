library(magrittr)
library(dplyr)
library(glmnet)

lf <- list.files("/home")
if ("snarles" %in% lf) (ddir <- "/home/snarles/stat312data")
if ("ubuntu" %in% lf) (ddir <- "/home/ubuntu/stat312data")
if ("rstudio" %in% lf) (ddir <- "/home/rstudio/stat312data")
list.files(ddir)

## questionnaire data
qdata <- as.matrix(read.csv(paste0(ddir, "/data.csv"), header = TRUE, sep = '\t')[, 1:163]) %>% scale()
filtered_resp <- qdata

## fMRI data
train_resp <- read.csv(paste0(ddir, "/train_resp_all.csv"), header = FALSE) %>%
  t()%>% scale()
good_vox <- (apply(train_resp, 2, function(v) sum(is.na(v))) == 0)
sum(good_vox)
filtered_resp <- train_resp[, good_vox]
dim(filtered_resp) #1750 22733

all_cache <- list()
n_train <- 100

setup_problem <- function(n_train = 100) {
  n_train <<- n_train
  inds_train <<- sample(dim(filtered_resp)[1], n_train)
  ind_pred <<- sample(dim(filtered_resp)[2], 1)
  x_tr <<- filtered_resp[inds_train, -ind_pred]
  x_te <<- filtered_resp[-inds_train, -ind_pred]
  y_tr <<- filtered_resp[inds_train, ind_pred]
  y_te <<- filtered_resp[-inds_train, ind_pred]
  current_pars <<- list(inds_train = inds_train, ind_pred = ind_pred)
  x <- rbind(x_tr, x_te)
  res_n <<- svd(x)
}

ridge_error <- function() {
  res_ridge <<- cv.glmnet(x_tr, y_tr, alpha = 0)
  #plot(res_ridge)
  y_hat_ridge <<- predict(res_ridge, x_te)
  (sqe_glmnet <<- sum((y_te - y_hat_ridge)^2))
}

pcr_error <- function(ks) {
  pcr_ks <<- ks
  pcr_errs <<- numeric(length(ks))
  inds_val <- sample(length(inds_train), 20)
  for (i in 1:length(ks)) {
    k <- ks[i]
    tt_tr1 <- x_tr[-inds_val, ] %*% res_n$v[, 1:k]
    tt_tr2 <- x_tr[inds_val, ] %*% res_n$v[, 1:k]
    res <- lm(y_tr[-inds_val] ~ tt_tr1)
    y_hat_pcr <- res$coefficients[1] + tt_tr2 %*% res$coefficients[-1]
    pcr_errs[i] <<- sum((y_hat_pcr - y_tr[inds_val])^2)
  }
  k_sel <- ks[order(pcr_errs)[1]]
  print(paste("selected k:", k_sel))
  tt_tr <- x_tr %*% res_n$v[, 1:k_sel]
  tt_te <- x_te %*% res_n$v[, 1:k_sel]
  res <- lm(y_tr ~ tt_tr)
  y_hat_pcr <- res$coefficients[1] + tt_te %*% res$coefficients[-1]
  (sqe_pcr <<- sum((y_hat_pcr - y_te)^2))
}

record_results <- function() {
  current_pars$sqe_glmnet <<- sqe_glmnet
  current_pars$sqe_pcr <<- sqe_pcr
  all_cache <<- c(all_cache, list(current_pars))
}

reset_results <- function() {
  all_cache <<- list()
}


plot(res_n$d)
reset_results()

for (i in 1:10) {
  setup_problem(50)
  #current_pars
  ridge_error()
  pcr_error(7)
  record_results()  
}

all_cache
