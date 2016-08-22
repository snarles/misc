dat <- read.csv("misc2/wmdata1.csv", header = FALSE)
colnames(dat) <- c("outcome","cell_type","treatment")
res <- lm(outcome ~ cell_type + treatment, data = dat)
anova(res)
cor(dat[dat[, 2]==0 & dat[, 3]==0, 1], dat[dat[, 2]==1 & dat[, 3]==0,1]) #  0.843771
cor(dat[dat[, 2]==0 & dat[, 3]==1, 1], dat[dat[, 2]==1 & dat[, 3]==1,1]) #  0.2303471

cor.test(dat[dat[, 2]==0 & dat[, 3]==0, 1], dat[dat[, 2]==1 & dat[, 3]==0,1]) # p=0.1562
cor.test(dat[dat[, 2]==0 & dat[, 3]==1, 1], dat[dat[, 2]==1 & dat[, 3]==1,1]) # p=0.7697


library(Hmisc)
rcorr(cbind(dat[dat[, 2]==0 & dat[, 3]==0, 1], dat[dat[, 2]==1 & dat[, 3]==0,1]), type = "pearson")

cA <- dat[dat[, 2]==0 & dat[, 3]==0, 1]
cB <- dat[dat[, 2]==1 & dat[, 3]==0, 1]
tA <- dat[dat[, 2]==0 & dat[, 3]==1, 1]
tB <- dat[dat[, 2]==1 & dat[, 3]==1, 1]

Y <- rbind(cbind(cA, cB), cbind(tA, tB))
colnames(Y) <- c("cellA", "cellB")
x <- c(rep(0, 4), rep(1, 4))
fit <- manova(Y ~ x)
summary(fit, test = "Wilks")
t.test(cA, tA)
t.test(cB, tB)
