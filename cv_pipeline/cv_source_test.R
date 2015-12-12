#####################################
##  Cross-validating ML pipelines  ##
#####################################

## Written by Charles Zheng, 2015
## URL: https://github.com/misc/cv_pipeline

source('cv_pipeline/cv_source.R')

library(pracma)
n <- 200
n_te <- 20
p <- 12
X <- randn(n, p)
X2 <- randn(n_te, p)

bt <- rnorm(p)
y <- X %*% bt + rnorm(n)
y2 <- X2 %*% bt + rnorm(n_te)

res <- lm(y ~ X)
predict(res, X2)

extract_high_variance(X)

extract_pcs(X)

train_data <- cbind(y, X)
test_data <- cbind(y2, X2)

test_error(train_data = train_data, test_data = test_data)