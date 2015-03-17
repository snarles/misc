## testing 

library(devtools)
install("zheng001")
library(zheng001)


cm <- diag(rep(1, 5))
gd <- new_gaussian(cm)

emp_rates <- numeric(100)
the_rates <- numeric(100)

for (k in 1:100) {
  params <- new_simulation_pars(gd, sigma=0.3, k=k)
  sr <- run_simulation(params, 1, 20)
  emp_rates[k] <- sr@id_rate
  the_rates[k] <- theoretical_rate(params)
}

plot(1:100, emp_rates)
lines(1:100, the_rates)
