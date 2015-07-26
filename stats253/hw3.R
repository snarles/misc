####
##  Charles Zheng HW 3
####


setwd("stats253")


library(maptools)
library(RColorBrewer)
library(classInt)
library(magrittr)
library(spdep)

shapes <- readShapeSpatial("scotlip/scotlip.shp")
plot(shapes)
View(shapes@data)

nb <- poly2nb(shapes)

shapes@data %>% names
#[1] "CODENO"    "AREA"      "PERIMETER" "RECORD_ID" "DISTRICT"  "NAME"      "CODE"      "CANCER"    "POP"      
#[10] "CEXP"      "AFF"    

shapes@data %$% plot(POP, CANCER)

