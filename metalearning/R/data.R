#' Computes prototypes
#' 
#' This function returns k prototype columns
#' @param tab Data frame
#' @param k Number of prototypes
prototypes <- function(tab, k) {
  colnames(tab)[protoclust::protocut(
    protoclust::protoclust(dist(t(tab))), k)$protos]
}

#' Class dataset
#' 
#' Represents a collection of related datasets with shared 'x' variables
#' @param x data frame of predictors
#' @param y vector of responses
#' @param tr_inds training set indices
dataset <- function(x, y, tr_inds) {
  temp <- list()
  temp$x_tr <- x[tr_inds, ]
  temp$y_tr <- y[tr_inds]
  temp$x_te <- x[-tr_inds, ]
  temp$y_te <- y[-tr_inds]
  temp$tr_inds <- tr_inds
  structure(temp, class = "dataset")
}

#' Class datasets
#' 
#' Represents a collection of related datasets with shared 'x' variables
#' @param alldata a data frame of all data
#' @param x_inds indices or names of x variables
#' @param y_inds indices or names of y variable per split
#' @param datasplits vector of indices indicating how to split the data
#' @param tr_frac fraction for training data
datasets <- function(alldata, x_inds, y_inds, datasplits, tr_frac = 0.5) {
  if (length(y_inds) == 1) y_inds <- rep(y_inds, length(unique(datasplits)))
  alldata <- alldata[!is.na(datasplits), ]
  datasplits <- datasplits[!is.na(datasplits)]
  x_data <- alldata[, x_inds]
  x_mu <- lapply(x_data, mean)
  x_sd <- lapply(x_data, sd)
  usplits <- sort(unique(datasplits))
  nsplits <- length(usplits)
  sets <- list(nsplits)
  for (i in 1:nsplits) {
    inds <- which(datasplits == usplits[i])
    sets[[i]] <- dataset(x_data[inds, , drop = FALSE], alldata[inds, y_inds[i]],
                        sample(length(inds), floor(tr_frac * length(inds))))
  }
  structure(list(x_mu = x_mu, x_sd = x_sd, nsplits = nsplits, sets = sets),
            class = "datasets")
}
