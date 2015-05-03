#' Creates a grid design data frame
#' 
#' @param data The original data frame
#' @param grid_inds The indices for the grid
#' @param reso The resolution of the grid
#' @return A data frame with the same names, but variables in grid_inds replaced by a grid
#' @export
grid_design <- function(data, grid_inds, reso = 20) {
  n <- dim(data)[1]
  p <- dim(data)[2]
  ans <- matrix(0, reso^(length(grid_inds)), p)
  ans[, grid_inds] <- as.matrix(AlgDesign::gen.factorial(reso, length(grid_inds), TRUE))
  for (i in grid_inds) {
    ans[, i] <- (ans[, i] - min(ans[, i]))/(max(ans[, i]) - min(ans[, i]))
    ans[, i] <- (max(data[, i]) - min(data[, i])) * ans[, i] + 
      min(data[, i])
  }
  colnames(ans) <- colnames(data)
  data.frame(ans)
}

#' MATLAB-style random normal matrix
#' 
#' @param n Dimension of the matrix
#' @param p Dimension of the matrix (default n)
#' @param colnames Whether to give colnames
#' @export
randn <- function(n, p = n, colnames = FALSE) {
  ans <- matrix(rnorm(n * p), n, p)
  if (colnames) colnames(ans) <- paste0("V", 1:p, sep = "")
  ans
}
#' Random uniform matrix
#' 
#' @param n Dimension of the matrix
#' @param p Dimension of the matrix (default n)
#' @param center If true, generates [-1, 1] random variates
#' @param colnames Whether to give colnames
#' @export
randu <- function(n, p = n, center = FALSE, colnames = FALSE) {  
  ans <- matrix(runif(n * p), n, p)
  if (center) ans <- 2 * (ans - .5)
  if (colnames) colnames(ans) <- paste0("V", 1:p, sep = "")
  ans
}

