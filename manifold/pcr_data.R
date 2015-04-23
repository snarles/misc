library(magrittr)
library(dplyr)
lf <- list.files("/home")
if ("snarles" %in% lf) (ddir <- "/home/snarles/stat312data")
if ("ubuntu" %in% lf) (ddir <- "/home/ubuntu/stat312data")
if ("rstudio" %in% lf) (ddir <- "/home/rstudio/stat312data")
list.files(ddir)

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
}

setup_problem(100)
current_pars

dim(x_tr)
dim(x_te)
