#####################################
##  Cross-validating ML pipelines  ##
#####################################

## Written by Charles Zheng, 2015
## URL: https://github.com/misc/cv_pipeline

source('cv_pipeline/cv_source.R')

library(pracma)
n <- 200
p <- 30
X <- randn(n, p)

extract_high_variance(X)

extract_pcs(X)
