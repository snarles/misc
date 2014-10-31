res0 <- 10000
len0 <- 10
zs <- len0 * (1:res0)/res0
fab <- function(a,b) {
  1/(a-b) * (exp(-zs/a)-exp(-zs/b))
}
a <- 1.2
b <- 1
ff <- fab(a,b)
empsampl <- a*rexp(res0) + b*rexp(res0)
x1 <- 1
x2 <- 2
empprob <- sum(empsampl > x1 & empsampl < x2)/res0
empprob
theprob <- sum(ff[zs > x1 & zs < x2])/(res0/len0)
theprob
