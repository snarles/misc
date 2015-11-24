####
##  Gradient descent ODE 
####

source("gp//graphics_source.R")
library(fields)
library(sfsmisc)

## GENERATE X GRID
xseq <- seq(-1, 1, by = 0.2)
xs <- cbind(rep(xseq, length(xseq)), rep(xseq, each = length(xseq)))

## GENERATE RF
lambda <- 2
k.basis <- 8
res_grf <- grf_grid(xs, lambda, k.basis)
res_grf %$% ctp(vals, d1, d2)

eps <- 0.1
plot(NA, NA, xlim =c(-1,1), ylim = c(-1, 1))
arrow.plot(xs[,1], xs[,2], Re(res_grf$d1), Re(res_grf$d2), arrowfun = p.arrows)

help(arrow.plot)
