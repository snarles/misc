#http://www.mrc-bsu.cam.ac.uk/wp-content/uploads/pigweights.txt

setwd("stats253/")

y=c(1,1,0,7,5,10,30,30,41,48,66,72,56,46,45,22,24,12,5,0,1)
n=522
s=21

data <- list(y=c(1,1,0,7,5,10,30,30,41,48,66,72,56,46,45,22,24,12,5,0,1),n=522,s=21)
inits <- list(gam=c(-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3))

gam=c(-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3)

library(rjags)

model <- jags.model(file="pig.bug", data = data, inits=inits, n.chains=5)
out <- coda.samples(model, "Sm", n.iter = 1000)
length(out)
dim(out[[1]])
matplot(out[[1]])
matplot(out[[1]], type = "l")

gd <- gelman.diag(out)
help(gelman.diag)

gd$psrf
names(gd)

traceplot(out[[1]][, 1])
