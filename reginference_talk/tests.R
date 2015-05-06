library(reginference)
library(magrittr)
library(dplyr)
library(glmnet)

## prostate data
data(prostate)
dim(prostate)
x <- as.matrix(prostate[, 1:8])
y <- prostate[, 9]
summary(lm(y ~ x))

## generate random noise columns
z <- matrix(rnorm(97 * 100), 97, 100)
x2 <- data.frame(cbind(x, z))
sigma_h <- sd(lm(y ~ as.matrix(x))$residuals)

cors <- cor(x2, y)
cors[order(-abs(cors)), , drop = F]

lars(x2, y)

library(covTest)
covTest(lars(x, y), x, y)
help(covTest)
res <- covTest(lars(x2, y), x2, y, sigma.est = sigma_h)
res




## galaxy data
data(galaxy)
names(galaxy)
gal_x <- galaxy[, 1:4]
gal_y <- galaxy[, 5]

## pf 16 data
## questionnaire data
ddir <- "/home/snarles/mldata"
rawdata <- read.csv(paste0(ddir, "/16PF/data.csv"), header = TRUE, sep = '\t')
q_inds <- 1:163
o_inds <- 164:169
pf_x <- rawdata[, q_inds]
pf_y <- rawdata$age
filt <- pf_y < 100
pf_x <- pf_x[filt, ]
pf_y <- pf_y[filt]
filtdata <- rawdata[filt, ]
dim(pf_x) # 49086   163
f1 <- as.formula(paste("age ~ ", paste(colnames(pf_x), collapse = " + ")))
f1
res_pf <- lm(f1, data = filtdata)
#res_pf <- lm(pf_y ~ as.matrix(pf_x))
cm0 <- summary(res_pf)$coefficients[-1, ]
cm <- cm0[order(cm0[, 1]), ]
png("reginference_talk/pf16_coefs.png", width = 800, height = 650)
par(mar = c (5.1, 7.1, 4.1, 2.1))
plot(1:163, cm[, 1], xlab = "variable", ylab = expression(beta[i]), axes = FALSE, cex.lab = 3)
abline(0, 0)
for (i in 1:163) {
  lines(c(i, i), cm[i, 1] + c(3, -3) * cm[i, 2], type = "o", pch = "-")
}
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))

cm0
signif <- (abs(cm0[, 1])  >  3 * cm0[, 2]) 
sum(signif)
min(abs(res_pf$coefficients[-1][signif]))
f2 <- as.formula(paste("age ~ ", paste(colnames(pf_x)[signif], collapse = " + ")))
f2
res_pf2 <- lm(f2, data = filtdata)
yh1 <- res_pf$coefficients[1] + as.matrix(pf_x) %*% res_pf$coefficients[-1]
yh2 <- res_pf$coefficients[1] + as.matrix(pf_x[, signif]) %*% res_pf$coefficients[-1][signif]

range(pf_y)
png("reginference_talk/pf16_modelcheck.png", width = 700, height = 700)
par(mar = c (5.1, 7.1, 4.1, 2.1))
plot(yh1, yh2, ylim = c(0, 80), xlim = c(0, 80), pch = "+",
     xlab = "full model", ylab = "sel. model", cex.lab = 2, cex.axis = 2)
abline(0, 1, col = "red", lwd = 2)
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))

subs <- pf_x[, sample(163, 15)]
res_sub <- lm(pf_y ~ as.matrix(subs))
cfs <- summary(res_sub)$coefficients[-1, ]
subsel <- abs(cfs[, 1]) > 3 * cfs[, 2]
sum(subsel)
png("reginference_talk/pf16_modelcheck2.png", width = 700, height = 700)
par(mar = c (5.1, 7.1, 4.1, 2.1))
yh1 <- res_sub$coefficients[1] + as.matrix(subs) %*% res_sub$coefficients[-1]
yh2 <- res_sub$coefficients[1] + as.matrix(subs[, subsel]) %*% res_sub$coefficients[-1][subsel]
plot(yh1, yh2, ylim = c(0, 80), xlim = c(0, 80), pch = "+",
     xlab = "full model", ylab = "sel. model", cex.lab = 2, cex.axis = 2)
abline(0, 1, col = "red", lwd = 2)
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))

sh <- cov(pf_x)
n <- dim(pf_x)[1]
vs <- diag(solve(sh))
plot(sort(vs))

hist(res_pf$residuals)
var(res_pf$residuals)
max(abs(res_pf$coefficients[-1]))
lapply(pf_x, var)
plot(res_pf$residuals, res_pf$fitted.values)
plot(res_pf$fitted.values, pf_y)
res <- loess(pf_y ~ res_pf$fitted.values)
plot(res$x, res$fitted)



## HIV data
drug_class = 'PI' # Possible drug types are 'PI', 'NRTI', and 'NNRTI'.
base_url = 'http://hivdb.stanford.edu/pages/published_analysis/genophenoPNAS2006'
gene_url = paste(base_url, 'DATA', paste0(drug_class, '_DATA.txt'), sep='/')
tsm_url = paste(base_url, 'MUTATIONLISTS', 'NP_TSM', drug_class, sep='/')

gene_df = read.delim(gene_url, na.string = c('NA', ''), stringsAsFactors = FALSE)
tsm_df = read.delim(tsm_url, header = FALSE, stringsAsFactors = FALSE)
names(tsm_df) = c('Position', 'Mutations')
head(tsm_df)
dim(tsm_df)

# Returns rows for which every column matches the given regular expression.
grepl_rows <- function(pattern, df) {
  cell_matches = apply(df, c(1,2), function(x) grepl(pattern, x))
  apply(cell_matches, 1, all)
}

pos_start = which(names(gene_df) == 'P1')
pos_cols = seq.int(pos_start, ncol(gene_df))
valid_rows = grepl_rows('^(\\.|-|[A-Zid]+)$', gene_df[,pos_cols])
gene_df = gene_df[valid_rows,]

# Flatten a matrix to a vector with names from concatenating row/column names.
flatten_matrix <- function(M, sep='.') {
  x <- c(M)
  names(x) <- c(outer(rownames(M), colnames(M),
                      function(...) paste(..., sep=sep)))
  x
}

# Construct preliminary design matrix.
muts = c(LETTERS, 'i', 'd')
X = outer(muts, as.matrix(gene_df[,pos_cols]), Vectorize(grepl))
X = aperm(X, c(2,3,1))
dimnames(X)[[3]] <- muts
X = t(apply(X, 1, flatten_matrix))
mode(X) <- 'numeric'

# Remove any mutation/position pairs that never appear in the data.
X = X[,colSums(X) != 0]

# Extract response matrix.
Y = gene_df[,4:(pos_start-1)]

dim(X) # 846 361
dim(Y) # 846   7

trans_mat <- function (X, y) {
  # Log-transform the drug resistance measurements.
  y = log(y)
  
  # Remove patients with missing measurements.
  missing = is.na(y)
  y = y[!missing]
  X = X[!missing,]
  
  # Remove predictors that appear less than 3 times.
  X = X[,colSums(X) >= 3]
  
  # Remove duplicate predictors.
  X = X[,colSums(abs(cor(X)-1) < 1e-4) == 1]
  
  list(X, y)
}

res1 <- trans_mat(X, Y[, 5])
HIV_x <- res1[[1]]
HIV_y <- res1[[2]]
res_HIV <- cv.glmnet(HIV_x, HIV_y, alpha = 1)
names(res_HIV)
res_HIV$glmnet.fit
yh <- predict(res_HIV, HIV_x)
hist(HIV_y - yh)
plot(yh, HIV_y)

## fMRI data
length(train_resp[1, ])
dim(feature_train)
fMRI_y <- as.matrix(train_resp)[1, ]
sum(is.na(feature_train))
res_fmri <- cv.glmnet(as.matrix(feature_train), fMRI_y, alpha = 0.5)
yh <- predict(res_fmri, feature_train)
plot(yh, fMRI_y)
hist(fMRI_y - yh)
lm((yh - fMRI_y)^2 ~ yh)
bt <- coef.cv.glmnet(res_fmri)
sum(bt != 0)
length(bt)
fMRI_x <- feature_train[, bt[-1] != 0]
dim(fMRI_x)
res_fmri <- lm(fMRI_y ~ as.matrix(fMRI_x))
plot(res_fmri$fitted.values, fMRI_y)
