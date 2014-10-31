# Charles Zheng
# Code for calculating simplex probabilities

empprob <- function(a,b,res0=10000) {
  m1 <- length(a)
  m2 <- length(b)
  mat1 <- matrix(rexp(m1*res0),res0,m1)
  mat2 <- matrix(rexp(m2*res0),res0,m2)
  sums <- mat1 %*% a - mat2 %*% b
  sum(sums > 0)/res0
}

calcCs <- function(a) {
  m <- length(a)
  ans <- numeric(m)
  for (i in 1:m) {
    ans[i] = a[i]^(m-1)/prod(a[i]-a[-i])
  }
  ans
}

centmat <- function(a,b) {
  mat <- matrix(0,length(a),length(b))
  matrix(a[row(mat)]/(a[row(mat)] + b[col(mat)]),length(a),length(b))
}

theprob <- function(a,b) {
  calcCs(a) %*% centmat(a,b) %*% calcCs(b)
}

# In this example I compute the probability that 1*X1+2*X2+3*X3-4*X4-5*X5-6*X6

a <- c(1,2,3)
b <- c(4,5,6)
empprob(a,b) # empirical probability from 10000 samples
theprob(a,b) # theoretical probability
