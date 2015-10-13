training.data.all <- readRDS("stats202/training.data.all")
library(leaps)
#regfit.full = regsubsets(ALSFRS_slope~.,data=training.data.all, nvmax=19, method="forward")

regfit.full = regsubsets(ALSFRS_slope~.,data=training.data.all, nvmax=0, method="forward")
rcopy <- training.data.all
dim(rcopy) # [1] 2424  859
for (i in 1:859) rcopy[[i]] <- as.numeric(rcopy[[i]])
table(sapply(rcopy, class))

regfit.full = regsubsets(ALSFRS_slope~.,data=rcopy, nvmax=0, method="forward")
for (i in 1:859) rcopy[[i]] <- rcopy[[i]] + 1e-5 * rnorm(2424)
regfit.full = regsubsets(ALSFRS_slope~.,data=rcopy, nvmax=0, method="forward")




for (i in 1:length(training.data.all)) {
  training.data.all[[i]] <- training.data.all[[i]] + 1e-5 * rnorm(dim(training.data.all)[1])
}
training.data.all$ALSFRS_slope <- training.data.all$ALSFRS_slope + 1e-5 * rnorm(2424)
regfit.full = regsubsets(ALSFRS_slope~.,data=training.data.all, nvmax=19, method="forward")
