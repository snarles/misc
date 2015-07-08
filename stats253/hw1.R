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

e <- resid_wet
D <- Dall[which(!is.na(snowpack$snow_wet)), which(!is.na(snowpack$snow_wet))]
cov.class <- exp2.cov.class


