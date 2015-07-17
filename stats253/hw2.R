####
##  Charles Zheng
####

####
##  Problem 1
####

edat0 <- read.csv("stats253/2012USElection.csv", sep = ",")
edat0$Obama.vote <- as.numeric(gsub(",", "", edat0$Obama.vote))
edat0$Romney.vote <- as.numeric(gsub(",", "", edat0$Romney.vote))
lapply(edat0, class)
#View(edat0[edat0$FIPS == 0, ])
filt <- !(edat0$State.Postal %in% c("AK", "HI") | edat0$FIPS == 0)
edat <- edat0[filt, ]
rownames(edat) <- paste(edat$County.Name, edat$FIPS)
dim(edat) # 4021 7
length(unique(edat$State.Postal)) # 41
setdiff(unique(edat0$State.Postal), unique(edat$State.Postal)) # AK DC HI

####
##  Problem 2
####

library(maptools)
library(RColorBrewer)
library(classInt)


shapes <- readShapeSpatial("stats253/UScounties/UScounties.shp")
class(shapes)
#shapes[[2]][1:10]
#plot(shapes)
#shapes
class(shapes)

#View(shapes@data)
rownames(shapes@data) <- paste(shapes@data$NAME, shapes@data$FIPS)
length(shapes@polygons)
shapes@polygons[[1]]
intersect(rownames(edat), rownames(shapes@data))
setdiff(rownames(edat), rownames(shapes@data))
setdiff(rownames(shapes@data), rownames(edat))

"Anchorage" %in% edat0$County.Name


edat2 <- edat[rownames(edat) %in% rownames(shapes@data), ]
shapes2 <- shapes[rownames(shapes@data) %in% rownames(edat), ]
rownames(shapes2@data) <- rownames(shapes@data)[rownames(shapes@data) %in% rownames(edat)]
for (i in 1:length(shapes2)) {
  shapes2@polygons[[i]]@ID <- rownames(shapes2@data)[i]
}



setdiff(rownames(edat2), rownames(shapes2@data))
setdiff(y = rownames(edat2), x =rownames(shapes2@data))

#trace(SpatialPolygonsDataFrame, browser)
edat3 <- SpatialPolygonsDataFrame(shapes2, edat2)
names(edat3)
plot(edat3["Obama.vote"], col = "grey")

pal <- brewer.pal(5, "Reds")
q5 <- classIntervals(edat3@data$Obama.vote, n = 5, style = "quantile")
plot(q5, pal = pal)


q5colors <- findColours(q5, pal)
plot(edat3, col = q5colors)
