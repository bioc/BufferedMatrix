\name{as.BufferedMatrix}
\alias{as.BufferedMatrix}
\alias{is.BufferedMatrix}
\title{Check or Coerce object to BufferedMatrix}
\description{'as.BufferedMatrix' will coerce the supplied object into a
  BufferedMatrix. 'is.BufferedMatrix' checks whether the supplied
  argument is a BufferedMatrix.
}
\usage{
 as.BufferedMatrix(x, bufferrows=1, buffercols=1,directory=getwd())
 is.BufferedMatrix(x)
}
\arguments{
  \item{x}{an R object}
  \item{bufferrows}{number of rows to be buffered if the row buffer is
    activated} 
  \item{buffercols}{number of columns to be buffered}
  \item{directory}{path to directory where temporary files should be stored}

}
\details{
  These functions are useful for converting between R
  \code{\link{matrix}} objects and
  \code{\link[BufferedMatrix:BufferedMatrix-class]{BufferedMatrix}} objects. 
}
\author{B. M. Bolstad <bmb@bmbolstad.com>}
\keyword{manip}
