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

sort(snowpack$snow_wet)
sort(snowpack$snow_dry)

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

temp_wet <- with(snowpack, data.frame(wet = 1, latitude = latitude[!is.na(snow_wet)],
                                      longitude = longitude[!is.na(snow_wet)],
                                      elevation = elevation[!is.na(snow_wet)],
                                      amount = snow_wet[!is.na(snow_wet)]))
temp_dry <- with(snowpack, data.frame(wet = 0, latitude = latitude[!is.na(snow_dry)],
                                      longitude = longitude[!is.na(snow_dry)],
                                      elevation = elevation[!is.na(snow_dry)],
                                      amount = snow_dry[!is.na(snow_dry)]))
reg_data <- rbind(temp_wet, temp_dry)
res_trend <- lm(amount ~ wet + latitude + longitude + elevation,
   data = reg_data)
summary(res_trend)

####
##  Plot residuals
####

resid <- res_trend$residuals
max(resid - min(resid))
st_resid <- floor((resid - min(resid)) * 170/(max(resid) - min(resid))) + 1

library(magrittr)

pdf("stats253/hw1_resid1.pdf")
layout(matrix(1:2, 1, 2))
reg_data %$% plot(latitude[wet == 1], longitude[wet == 1],
                  col = depthcol[st_resid[wet == 1]])
title("wet")
reg_data %$% plot(latitude[wet == 0], longitude[wet == 0],
                  col = depthcol[st_resid[wet == 0]])
title("dry")
dev.off()

####
##  Calculate distances
####

library(geosphere)
library(pracma)
n <- dim(snowpack)[1]
D <- zeros(n, n)
D <- distGeo(as.matrix(snowpack[, 2:1]), as.matrix(snowpack[, 2:1]))

