% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/negatives.R
\name{random_nw_surface}
\alias{random_nw_surface}
\title{Nonlinear surface}
\usage{
random_nw_surface(x, k, h = 1, handle = TRUE, normalize = TRUE)
}
\arguments{
\item{x}{The data matrix}

\item{k}{Number of control points}

\item{h}{Bandwidth (default 1)}

\item{handle}{Return a function handle? If FALSE, returns the function values at x}

\item{normalize}{Adjust the function to have unit variance, zero mean, and zero regression coeffs}
}
\value{
A function handle if \code{handle = TRUE} and a vector if \code{handle = FALSE}
}
\description{
A random instance of \code{nw_surface}
}

