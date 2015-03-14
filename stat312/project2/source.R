# a generic function
# returns d x N matrix of points
sample_unif <- function(omega, n = 1) {
  t(rep(NA, n))
}

setClass("parameter_space")
setClass(
  "ball", 
  representation(
    radius = "numeric",
    dimension = "numeric"),
  contains = "parameter_space"
)

setMethod(
  "sample_unif",
  signature(omega = "ball", n = "numeric"),
  function(omega, n) {
    m <- matrix(rnorm(n * omega@dimension), omega@dimension, n)
    m <- apply(m, 2, function(v) v / sqrt(sum(v^2)))
    m * omega@radius
  }
)


setClass("distribution",
         representation(space = "parameter_space",
                        density = "function",
                        normalizing_constant = "numeric"))

setMethod("initalize", "distribution",
          function(.Object, space, density, ...) {
            normalizing_constant = 1
            callNextMethod(.Object,
                           space = space,
                           density = density,
                           normalizing_constant = normalizing_constant)
          })

one <- function(x) {
  1
}
b1 <- new("ball", radius = 2, dimension = 2)
