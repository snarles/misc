####
## Charles Zheng
## STATS 253
## HW 1
####

####
##  Snowpack plots
#### 

snowpack <- read.table("stats253/snowpack.csv", sep = ",", header = TRUE)
elevations <- read.table("stats253/snowpack_elevations.csv", sep = ",", header = TRUE)
snowpack <- cbind(snowpack, elevation = elevations$elevation)

names(snowpack)

depthcol <- hsv(0.5, 1, 0:170/170)

#sort(snowpack$snow_wet)
#sort(snowpack$snow_dry)

pdf("stats253/hw1_wet_dry.pdf")
layout(matrix(1:2, 1, 2))
plot(snowpack[!is.na(snowpack$snow_wet), 1:2], col = depthcol[snowpack$snow_wet + 1])
title("wet")
plot(snowpack[!is.na(snowpack$snow_dry), 1:2], col = depthcol[snowpack$snow_dry + 1])
title("dry")
dev.off()

layout(1)
plot(1:5)

####
##  Model trend
#### 

res_wet <- lm(snow_wet ~ latitude + longitude + elevation, data = snowpack)
summary(res_wet)
length(res_wet$residuals) # 153

res_dry <- lm(snow_dry ~ latitude + longitude + elevation, data = snowpack)
summary(res_dry)
length(res_dry$residuals) # 150

####
##  Plot residuals
####

resid_wet <- res_wet$residuals
resid_dry <- res_dry$residuals
resid <- c(resid_wet, resid_dry)

st_resid <- floor((resid - min(resid)) * 170/(max(resid) - min(resid))) + 1
st_wet <- st_resid[1:153]
st_dry <- st_resid[-(1:153)]

library(magrittr)

pdf("stats253/hw1_resid1.pdf")
layout(matrix(1:2, 1, 2))
snowpack %$% plot(latitude[!is.na(snow_wet)], longitude[!is.na(snow_wet)],
                  col = depthcol[st_wet])
title("wet")
snowpack %$% plot(latitude[!is.na(snow_dry)], longitude[!is.na(snow_dry)],
                  col = depthcol[st_dry])
title("dry")
dev.off()

####
##  Calculate distances
####

library(geosphere)
library(pracma)
n <- dim(snowpack)[1]
Dall <- zeros(n, n)
for (i in 1:n) {
  Dall[i, ] <- distGeo(as.matrix(snowpack[i, 2:1]), as.matrix(snowpack[, 2:1]))/1e5
}

e_wet <- resid_wet
D_wet <- Dall[which(!is.na(snowpack$snow_wet)), which(!is.na(snowpack$snow_wet))]
e_dry <- resid_dry
D_dry <- Dall[which(!is.na(snowpack$snow_dry)), which(!is.na(snowpack$snow_dry))]

####
##  Estimate covariances
####

source("stats253/covariance.R")
cov.class <- exp2.cov.class

pdf("stats253/hw1_cov_wet.pdf")
cov_wet <- estimate.cov.fun(e_wet, D_wet, cov.class)
title("Cov Wet")
dev.off()

pdf("stats253/hw1_cov_dry.pdf")
cov_dry <- estimate.cov.fun(e_dry, D_dry, cov.class)
title("Cov Dry")
dev.off()

####
##  Make predictions
####

X_wet <- model.matrix(res_wet)
X_dry <- model.matrix(res_dry)
temp <- 1:dim(snowpack)[1]
X_all <- model.matrix(temp ~ latitude + longitude + elevation, data = snowpack)

pre_wet <- with(snowpack, gls(y = snow_wet[!is.na(snow_wet)], X = X_wet, 
                              Sigma = cov_wet(D_wet),
                              SigmaX0_X = cov_wet(Dall[, !is.na(snow_wet)]),
                              X0 = X_all))

pre_dry <- with(snowpack, gls(y = snow_dry[!is.na(snow_dry)], X = X_dry, 
                              Sigma = cov_dry(D_dry) + 0.01 * eye(150),
                              SigmaX0_X = cov_dry(Dall[, !is.na(snow_dry)]),
                              X0 = X_all))

predictions <- snowpack
predictions$snow_wet[is.na(snowpack$snow_wet)] <- pre_wet[is.na(snowpack$snow_wet)]
predictions$snow_dry[is.na(snowpack$snow_dry)] <- pre_wet[is.na(snowpack$snow_dry)]

rr <- c(predictions$snow_wet, predictions$snow_dry)
st_rr <- floor((rr - min(rr))/(max(rr) - min(rr)) * 170) + 1
st_wet <- st_rr[1:260]
st_dry <- st_rr[-(1:260)]
length(depthcol)

pdf("stats253/hw1_predict1.pdf")
layout(matrix(1:2, 1, 2))
snowpack %$% plot(latitude, longitude,
                  col = depthcol[st_wet])
snowpack %$% points(latitude[is.na(snow_wet)], longitude[is.na(snow_wet)],
                  cex = .8, pch = "+")
title("wet: predictions with +")
snowpack %$% plot(latitude, longitude,
                  col = depthcol[st_dry])
snowpack %$% points(latitude[is.na(snow_dry)], longitude[is.na(snow_dry)],
                  cex = .8, pch = "+")
title("dry: predictions with +")
dev.off()

####
##  CV
####

pre_snowpack <- function(snowpack) {
  res_wet <- lm(snow_wet ~ latitude + longitude + elevation, data = snowpack)  
  res_dry <- lm(snow_dry ~ latitude + longitude + elevation, data = snowpack)
  resid_wet <- res_wet$residuals
  resid_dry <- res_dry$residuals
  e_wet <- resid_wet
  D_wet <- Dall[which(!is.na(snowpack$snow_wet)), which(!is.na(snowpack$snow_wet))]
  e_dry <- resid_dry
  D_dry <- Dall[which(!is.na(snowpack$snow_dry)), which(!is.na(snowpack$snow_dry))]
  cov_wet <- estimate.cov.fun(e_wet, D_wet, cov.class, plot = FALSE)
  cov_dry <- estimate.cov.fun(e_dry, D_dry, cov.class, plot = FALSE)
  X_wet <- model.matrix(res_wet)
  X_dry <- model.matrix(res_dry)
  pre_wet <- with(snowpack, gls(y = snow_wet[!is.na(snow_wet)], X = X_wet, 
                                Sigma = cov_wet(D_wet) + 0.001 * eye(dim(D_wet)[1]),
                                SigmaX0_X = cov_wet(Dall[, !is.na(snow_wet)]),
                                X0 = X_all))
  pre_dry <- with(snowpack, gls(y = snow_dry[!is.na(snow_dry)], X = X_dry, 
                                Sigma = cov_dry(D_dry) + 0.001 * eye(dim(D_dry)[1]),
                                SigmaX0_X = cov_dry(Dall[, !is.na(snow_dry)]),
                                X0 = X_all))
  predictions <- snowpack
  predictions$snow_wet[is.na(snowpack$snow_wet)] <- pre_wet[is.na(snowpack$snow_wet)]
  predictions$snow_dry[is.na(snowpack$snow_dry)] <- pre_wet[is.na(snowpack$snow_dry)]
  predictions
}


errs_wet <- numeric()
errs_dry <- numeric()

for(i in 1:10) {
  te_wet <- sample(which(!is.na(snowpack$snow_wet)), 5)
  te_dry <- sample(which(!is.na(snowpack$snow_dry)), 5)
  snowpack2 <- snowpack
  snowpack2$snow_wet[te_wet] <- NA
  snowpack2$snow_dry[te_dry] <- NA
  predictions2 <- pre_snowpack(snowpack2)
  err_wet <- mean((predictions2$snow_wet[te_wet] - snowpack$snow_wet[te_wet])^2)
  err_dry <- mean((predictions2$snow_dry[te_dry] - snowpack$snow_dry[te_dry])^2)
  errs_wet <- c(errs_wet, err_wet)
  errs_dry <- c(errs_wet, err_dry)
}

mean(errs_wet)
mean(errs_dry)
