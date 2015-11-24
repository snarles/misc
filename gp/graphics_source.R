library(lineId)
library(magrittr)

####
##  Random deformation generation
####

grf_zs <- function(lambda = 2, k.basis = 5) {
  kseq <- (-k.basis):k.basis
  kgrid <- cbind(rep(kseq, each=(2*k.basis + 1)), rep(kseq, (2*k.basis + 1)))
  klen <- dim(kgrid)[1]
  zs <- rnorm(klen) + 1i * rnorm(klen)
  zs <- zs * exp(-lambda/4 * rowSums(kgrid^2))
  zs  
}

## grf on points with coordinates x, cov = exp(-lambda d(x_i, x_j)^2/2)
grf_grid <- function(xs, lambda = 2, k.basis = 5) {
  kseq <- (-k.basis):k.basis
  kgrid <- cbind(rep(kseq, each=(2*k.basis + 1)), rep(kseq, (2*k.basis + 1)))
  klen <- dim(kgrid)[1]
  zs <- rnorm(klen) + 1i * rnorm(klen)
  zs <- zs * exp(-lambda/4 * rowSums(kgrid^2))
  mat <- apply(kgrid, 1, function(kk) {
    exp(1i * pi * colSums(kk * t(xs)))
  })
  dmat1 <- t(t(mat) * kgrid[, 1] * 1i * pi)
  dmat2 <- t(t(mat) * kgrid[, 2] * 1i * pi)
  vals <- (mat %*% zs)[, 1]
  d1 <- (dmat1 %*% zs)[, 1]
  d2 <- (dmat2 %*% zs)[, 1]
  list(vals = vals, d1 = d1, d2 = d2, zs = zs)
}

circ_confine <- function(xs, vals, d1, d2, pow = 1) {
  xtx <- rowSums(xs^2)
  gfunc <- (xtx^pow - 1)^2
  g1 <- 4 *(xtx - 1) * pow * xtx^(pow-1) * xs[, 1]
  g2 <- 4 *(xtx - 1) * pow * xtx^(pow-1) * xs[, 2]
  gfunc[xtx > 1] <- 0;
  g1[xtx > 1] <- 0; g2[xtx > 1] <- 0  
  new_vals <- vals * gfunc
  new_d1 <- vals * g1 + gfunc * d1
  new_d2 <- vals * g2 + gfunc * d2
  list(vals = new_vals, d1 = new_d1, d2 = new_d2)
}

ctp <- function(vals, d1, d2) {
  contour(xseq, xseq, matrix(Re(vals), nrow=length(xseq) ), lwd = 2)
  contour(xseq, xseq, matrix(Re(d1), nrow=length(xseq)) , add=TRUE, col="red")
  contour(xseq, xseq, matrix(Re(d2), nrow=length(xseq) ), add=TRUE, col="blue")  
}

cc_query_ <- function(zs, k.basis, pow) {
  kseq <- (-k.basis):k.basis
  kgrid <- cbind(rep(kseq, each=(2*k.basis + 1)), rep(kseq, (2*k.basis + 1)))
  ansf <- function(x) {
    ## rf vals and ds
    ef_vals <- exp(1i * pi * kgrid %*% x)[, 1]
    rf_val <- sum(zs * ef_vals)
    rf_d1 <- sum(1i*pi*kgrid[,1] * zs * ef_vals)
    rf_d2 <- sum(1i*pi*kgrid[,2] * zs * ef_vals)
    ## confine vals and ds
    xtx <- sum(x^2)
    gfunc <- (xtx^pow - 1)^2
    g1 <- 4 *(xtx - 1) * pow * xtx^(pow-1) * x[1]
    g2 <- 4 *(xtx - 1) * pow * xtx^(pow-1) * x[2]
    val <- gfunc * rf_val
    d1 <- gfunc * rf_d1 + g1 * rf_val
    d2 <- gfunc * rf_d2 + g2 * rf_val
    c(val = val, d1 = d1, d2 = d2)
  }
  ansf
}

####
##  Random shape generation
####
polycirc <- function(ncirc, tres = 100, radmax = 0.5,
                     innmax = 0.99, rad_dec = 0.1,
                     ina = 1, inb = 1, ra = 1, rb = 1) {
  inncen <- innmax * rbeta(ncirc, ina, inb)
  centers0 <- do.call(rbind, lapply(1:ncirc,
                     function(i) {
                       x <- rnorm(2)
                       x <- x/sqrt(sum(x^2))
                       x <- inncen[i] * x
                     }))
  radii0 <- rbeta(ncirc, ra, rb) * pmin(radmax, (innmax - inncen))
  ts <- seq(0, 2 * pi, length.out=tres)
  pts0 <- cbind(cos(ts), sin(ts))
  polys1 <- lapply(1:ncirc, function(i) {
    t(t(pts0) * radii0[i] + centers0[i, ])
  })
  polys2 <- lapply(1:ncirc, function(i) {
    t(t(pts0) * pmax(0, radii0[i]-rad_dec) + centers0[i, ])
  })  
  list(polys1, polys2)
}

deform_map <- function(polys, query, eps=0.01, nits=10, mp = TRUE) {
  for (i in 1:nits) {
    for (i in 1:length(polys)) {
      pts <- polys[[i]]
      ds <- t(apply(pts, 1, query))
      if (mp) {
        pts <- pts + eps * Re(cbind(ds[,3], -ds[, 2]))        
      } else {
        pts <- pts + eps * Re(cbind(ds[,2], ds[, 3]))                
      }
      polys[[i]] <- pts
    }    
  }
  polys
}

rand_deform <- function(polys, lambda = 8, k.basis = 8,
                        eps=0.01, nits=10, mp = TRUE) {
  zs <- grf_zs(lambda, k.basis)
  query <- cc_query_(zs, k.basis, pow)
  deform_map(polys, query, eps, nits, mp)
}

plot_polys <- function(polys, ...) {
  for (pts in polys) polygon(pts, ...)
}


