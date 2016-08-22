dat <- read.csv("misc2/wmdata1.csv", header = FALSE)
colnames(dat) <- c("outcome","cell_type","treatment")
res <- lm(outcome ~ cell_type + treatment, data = dat)
anova(res)
