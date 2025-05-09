# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title
#' fastPdist2
#' @description
#' Pairwise distance functions
#' 
#' @param Ar first matrix
#' 
#' @param Br second matrix
#' 
#' @details
#' \code{fastPdist} from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/
#'
#' @export
fastPdist2 <- function(Ar, Br) {
    .Call('zheng001_fastPdist2', PACKAGE = 'zheng001', Ar, Br)
}

