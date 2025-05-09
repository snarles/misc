% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/negatives.R
\name{noised_projections}
\alias{noised_projections}
\title{Create artificial, known negative variables}
\usage{
noised_projections(x, sigma = 0.1, q = dim(x)[2], name = NULL,
  adjoin = FALSE)
}
\arguments{
\item{x}{The (n x p) matrix or data frame X}

\item{sigma}{The noise level relative to the norm of the new columns}

\item{q}{The number of columns to be generated this way}

\item{name}{The column name}

\item{adjoin}{Whether or not to adjoin the result z to x}
}
\value{
An (n x q) matrix or data frame of random projections.  The class of the output matches the class of the input.
}
\description{
From a matrix X, produces random projections of X with added gaussian noise
}

