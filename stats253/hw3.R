####
##  Charles Zheng HW 3
####


setwd("stats253")


library(maptools)
library(RColorBrewer)
library(classInt)
library(magrittr)
library(spdep)
library(pracma)

shapes <- readShapeSpatial("scotlip/scotlip.shp")
plot(shapes)
View(shapes@data)

nb <- poly2nb(shapes)

shapes@data %>% names
#[1] "CODENO"    "AREA"      "PERIMETER" "RECORD_ID" "DISTRICT"  "NAME"      "CODE"      "CANCER"    "POP"      
#[10] "CEXP"      "AFF"    
shapes@data %$% plot(POP, CANCER)

pairs(shapes@data)

## prune empty NB
shapes@data %>% dim
length(nb)
sapply(nb, length)

help(nb2mat)
W <- nb2mat(nb, zero.policy=TRUE)
W[1:10, 1:10]
rowSums(W)
diag(W)
W2 <- (W * lower.tri(W)) %>% {. + t(.)}
W2[1:10, 1:10]
res <- eigen(W2)
plot(res$values)
sort(res$values)

V <- res$vectors
d <- res$values
f2 <- function(x, y = 0) sum((x-y)^2)
f2(W2, V %*% (d * t(V)))

n <- dim(W)[1]
phi <- 0.1
f2(solve(eye(n) - phi * W2), V %*% (1/(1 - phi * d) * t(V)))

y <- shapes@data$CANCER
X <- shapes@data %$% cbind(POP, CEXP, AFF, AREA, PERIMETER)
colnames(X)






