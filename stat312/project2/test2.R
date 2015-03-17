## testing 

library(devtools)
library(roxygen2)
install("zheng001")
library(zheng001)


calc_emp_rates <- function(d, sigma, ks, n1, n2) {
  cm <- diag(rep(1, d))
  gd <- new_gaussian(cm)
  emp_rates <- numeric(length(ks))
  ses <- numeric(length(ks))
  for (i in 1:length(ks)) {
    params <- new_simulation_pars(gd, sigma, ks[i])    
    sr <- run_simulation(params, n1, n2)
    emp_rates[i] <- sr@id_rate
    ses[i] <- sd(sr@id_rates)/sqrt(n1)
  }
  list(emp_rates, ses)
}

calc_the_rates <- function(d, sigma, ks) {
  cm <- diag(rep(1, d))
  gd <- new_gaussian(cm)
  de <- density_at(gd, sample_points(gd, 1e5))
  the_rates <- numeric(length(ks))
  for (i in 1:length(ks)) {
    params <- new_simulation_pars(gd, sigma, ks[i])    
    the_rates[i] <- theoretical_rate(params)
  }
  the_rates  
}

pdf("paper/plot1_3.pdf")
ks_emp <- 1:20 * 200
ks_the <- 1:200 * 20
plot(ks_the, calc_the_rates(3, .2, ks_the), type="l", ylim = c(0, 1),
     xlab="k", ylab = "id. rate")
res <- calc_emp_rates(3, .2, ks_emp, 100, 100)
points(ks_emp, res[[1]] + res[[2]],
     ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
points(ks_emp, res[[1]] - res[[2]],
       ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
for (i in 1:length(ks_emp)) {
  lines(rep(ks_emp[i], 2), res[[1]][i] + res[[2]][i] * c(-1, 1))
}
title("Dimension 3, sigma = 0.2")
dev.off()

pdf("paper/plot1_6.pdf")
ks_emp <- 1:20 * 200
ks_the <- 1:200 * 20
plot(ks_the, calc_the_rates(6, .4, ks_the), type="l", ylim = c(0, 1),
     xlab="k", ylab = "id. rate")
res <- calc_emp_rates(6, .4, ks_emp, 100, 100)
points(ks_emp, res[[1]] + res[[2]],
       ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
points(ks_emp, res[[1]] - res[[2]],
       ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
for (i in 1:length(ks_emp)) {
  lines(rep(ks_emp[i], 2), res[[1]][i] + res[[2]][i] * c(-1, 1))
}
title("Dimension 6, sigma = 0.4")
dev.off()

pdf("paper/plot1_10.pdf")
ks_emp <- 1:20 * 200
ks_the <- 1:200 * 20
plot(ks_the, calc_the_rates(10, .5, ks_the), type="l", ylim = c(0, 1),
     xlab="k", ylab = "id. rate")
res <- calc_emp_rates(10, .5, ks_emp, 100, 100)
points(ks_emp, res[[1]] + res[[2]],
       ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
points(ks_emp, res[[1]] - res[[2]],
       ylim=c(0, 1), xlab="k", ylab = "id. rate", pch="-")
for (i in 1:length(ks_emp)) {
  lines(rep(ks_emp[i], 2), res[[1]][i] + res[[2]][i] * c(-1, 1))
}
title("Dimension 10, sigma = 0.5")
dev.off()



