####
##  Charles Zheng
####

####
##  Problem 1
####

edat0 <- read.csv("stats253/2012USElection.csv", sep = ",")
#View(edat0[edat0$FIPS == 0, ])
filt <- !(edat0$State.Postal %in% c("AK", "HI") | edat0$FIPS == 0)
edat <- edat0[filt, ]
dim(edat) # 4021 7
length(unique(edat$State.Postal)) # 41
setdiff(unique(edat0$State.Postal), unique(edat$State.Postal)) # AK DC HI

####
##  Problem 2
####

library(maptools)
shapes <- readShapeSpatial("stats253/UScounties/UScounties.shp")
shapes[[2]][1:10]
plot(shapes)
shapes
class(shapes)

View(shapes@data)
length(shapes@polygons)
shapes@polygons[[1]]
