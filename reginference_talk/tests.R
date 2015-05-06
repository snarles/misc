library(reginference)
library(magrittr)
library(dplyr)
library(glmnet)



## galaxy data
data(galaxy)
names(galaxy)
gal_x <- galaxy[, 1:4]
gal_y <- galaxy[, 5]

## pf 16 data
## questionnaire data
lf <- list.files("/home")
if ("snarles" %in% lf) (ddir <- "/home/snarles/mldata")
rawdata <- read.csv(paste0(ddir, "/16PF/data.csv"), header = TRUE, sep = '\t')
q_inds <- 1:163
o_inds <- 164:169
pf_x <- rawdata[, q_inds]
pf_y <- rawdata$age
dim(pf_x) # 49159   163
res_pf <- lm(pf_y ~ as.matrix(pf_x))

