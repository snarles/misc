#' Transfer functions for neural networks
#' 
#' Thus function returns a specified transfer function
#' @param tranfer A string indicating the name of the transfer function
#' @export
transfer_functions <- function(transfer = c("logistic", "ppart", "sign"))
{
  transfer <- match.arg(transfer)
  switch(transfer,
         logistic = function(x) { exp(x)/(1 + exp(x)) },
         ppart = function(x) { x * (x > 0) },
         sign = sign)
}


#' Creates a random function with neural netowrk structure
#' 
#' This function returns a closure with given input dimensionality,
#' using a neural network with random weights
#' @param nunits
#' A vector indicating the number of layers in each unit, the first element being the input dimension
#' NOTE: Omit the 1-unit output layer
#' @param vars Marginal variance of coefficients for each layer, default = 1
#' @param sparsities Sparsity of coefficients for each layer, defaul = ndim
#' @param transfer Choice of transfer function
#' @keywords neural network, perceptron
#' @export
#' @examples
#' nunits <- c(5, 10)
#' x <- matrix(rnorm(100 * 5), 100, 5)
#' f <- random_ann(nunits)
#' y <- f(x)

random_ann <- function(nunits, vars = 0 * nunits + 1,
                       sparsities = nunits,
                       transfer = c("logistic", "ppart", "sign"))
{
  transfer <- match.arg(transfer)
  current_transfer <- transfer_functions(transfer)
  nlayers <- length(nunits)
  nunits <- c(nunits, 1)  ## the output layer has 1 unit
  weights <-  # each element collects the weights in one layer of the network
    lapply(1:nlayers, function(i_layer) {
      lapply(1:nunits[i_layer + 1], function(i_unit) {
        random_linear(ndim = nunits[i_layer], var = vars[i_layer],
                      sparsity = sparsities[i_layer])
      })
    })
  ans <- function(x) {
    if (is.null(dim(x))) x <- t(x)
    ndata <- dim(x)[1]
    activations <- list()
    activations[[1]] <- x
    for (i_layer in 1:nlayers) {
      new_act <- matrix(0, ndata, nunits[i_layer + 1]) # activations for current layer
      for (i_unit in 1:nunits[i_layer + 1]) {
        new_act[, i_unit] <- weights[[i_layer]][[i_unit]](activations[[i_layer]])
      }
      if (i_layer < nlayers) new_act <- apply(new_act, 2, current_transfer)
      if (is.null(dim(new_act))) new_act <- t(new_act)
      activations[[i_layer + 1]] <- new_act
    }
    as.numeric(activations[[nlayers + 1]])
  }
  ans
}