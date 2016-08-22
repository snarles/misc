dat <- read.csv("misc2/wmdata1.csv", header = FALSE)
colnames(dat) <- c("outcome","cell_type","treatment")
res <- lm(outcome ~ cell_type + treatment, data = dat)
anova(res)
cor(dat[dat[, 2]==0 & dat[, 3]==0, 1], dat[dat[, 2]==1 & dat[, 3]==0,1]) #  0.843771
cor(dat[dat[, 2]==0 & dat[, 3]==1, 1], dat[dat[, 2]==1 & dat[, 3]==1,1]) #  0.2303471
