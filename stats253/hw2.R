####
##  Charles Zheng
####


setwd("stats253")

####
##  Problem 1
####

edat0 <- read.csv("PrezElection2012.csv", sep = ",")
#edat0$Obama.vote <- as.numeric(gsub(",", "", edat0$Obama.vote))
#edat0$Romney.vote <- as.numeric(gsub(",", "", edat0$Romney.vote))
sapply(edat0, class)
#View(edat0[edat0$FIPS == 0, ])
filt <- !(edat0$State %in% c("AK", "HI", "DC") | edat0$FIPS == 0)
edat <- edat0[filt, ]
#rownames(edat) <- paste(edat$Name, edat$FIPS)
rownames(edat) <- paste0("FIPS", edat$FIPS)

dim(edat) # 3108 6
length(unique(edat$State)) # 48
setdiff(unique(edat0$State), unique(edat$State)) # AK DC HI
names(edat)
## [1] "State"      "FIPS"       "Name"       "TotalVotes" "Obama"      "Romney"    

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
#rownames(shapes@data) <- paste(shapes@data$NAME, shapes@data$FIPS)
sapply(shapes@data, class)
sfips <- as.numeric(as.character(shapes@data$FIPS))
rownames(shapes@data) <- paste0("FIPS", sfips)
length(shapes@polygons)
shapes@polygons[[1]]

## Joining edat and shapes
dim(edat)
length(unique(edat$FIPS))
length(unique(shapes@data$FIPS))
length(unique(intersect(edat$FIPS, shapes@data$FIPS)))

#intersect(rownames(edat), rownames(shapes@data))

setdiff(rownames(edat), rownames(shapes@data)) # character(0)
setdiff(rownames(shapes@data), rownames(edat)) # [1] "FIPS11001" "FIPS2185"...

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
#plot(edat3["Obama"], col = "grey")

pal <- brewer.pal(5, "Blues")
q5 <- classIntervals(edat3@data$Obama, n = 5, style = "quantile")
q5colors <- findColours(q5, pal)
plot(edat3, col = q5colors)


####
##  Problem 3
####

library(spdep)
adj <- read.table("county_adjacency.txt", sep = "", header = FALSE, fill = TRUE)
#plot(is.na(adj[1:100, 4]), pch = "|")
filler_inds <- which(!is.na(adj[, 4]))
filled_inds <- cumsum(!is.na(adj[, 4]))
#plot(filled_inds[1:100], type = "l")
adj[, 3:4] <- adj[filler_inds[filled_inds], 3:4]
#View(adj)
## Found an misentered row
adj[9629, 1:2] <- adj[9629, 2:1]

## match up FIPS between
#names(edat)
adj[, 2] <- as.numeric(as.character(adj[, 2]))
adj[, 4] <- as.numeric(as.character(adj[, 4]))
length(unique(adj[, 2])) # 3234
length(unique(adj[, 4])) # 1128


dim(adj)

temp0 <- c(as.character(adj[, 2]), as.character(adj[, 4]))
# which(is.na(as.numeric(temp0))) # 9629
# temp0[9629]
# adj[9629, ]

afips <- sort(unique(as.numeric(temp0)))
## Define the mapping from integers to FIPS
efips <- sort(unique(edat$FIPS))
fips2int <- numeric(max(efips))
fips2int[efips] <- 1:length(efips)
enames <- paste0("FIPS", efips)
names(fips2int)[efips] <- enames

length(afips) # 3234

length(unique(edat$FIPS)) # 3108
length(intersect(unique(edat$FIPS), afips)) # 3108

## Filter out guys in adj
filt <- (adj[, 2] %in% efips) & (adj[, 4] %in% efips) & (adj[, 2] != adj[, 4])
adjf <- adj[filt, ]
dim(adjf) # 21139 4

## Symmetrize neighbor list
adjfs <- adjf; names(adjfs) <- names(adjf)[c(3, 4, 1, 2)]
adj2 <- rbind(adjf, adjfs)
dim(adj2)
length(unique(adj2[, 2])) # 3107
sort(setdiff(efips, unique(adj2[, 2]))) # 51510
fips2int[51510] # 2881
nb0 <- tapply(fips2int[adj2[, 4]], enames[fips2int[adj2[, 2]]], c)
length(nb0) # 3107
nb0 <- sapply(nb0, unique)
## These counties only had neighbors outside the census
empties <- fips2int[setdiff(enames, names(nb0))]
length(empties) # 1
nb1 <- as.list(numeric(3108))
nb1[fips2int[names(nb0)]] <- nb0
nb1[[2881]] <- 2881
length(nb1)
nb1 <- sapply(nb1, sort)
nb1 <- sapply(nb1, as.integer)


nb2 <- structure(nb1, class = "nb", region.id = enames,
                 GeoDa = list(shpfile = "unknown", ind = "unknown"),
                 gal = TRUE, call = TRUE, sym = TRUE)
lw <- nb2listw(nb2)

####
##  Problem 4
####

res1 <- spautolm(Obama ~ 1, data = edat, listw=lw)
summary(res1)

edat["fitted1"] <- res1$fit$fitted.values
edat["resid1"] <- res1$fit$residuals

plot_edat <- function(edat, variable, colr = "Blues") {
  edat3 <- SpatialPolygonsDataFrame(shapes2, edat)
  pal <- brewer.pal(5, colr)
  q5 <- classIntervals(edat3@data[, variable], n = 5, style = "quantile")
  q5colors <- findColours(q5, pal)
  plot(edat3, col = q5colors)
}

plot_edat(edat, "fitted1")
plot_edat(edat, "resid1", "Reds")






