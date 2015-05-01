#' Creates a grid design data frame
#' 
#' @param data The original data frame
#' @param grid_inds The indices for the grid
#' @param reso The resolution of the grid
#' @return A data frame with the same names, but variables in grid_inds replaced by a grid
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