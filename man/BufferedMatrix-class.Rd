\name{BufferedMatrix-class}
\docType{class}
\alias{BufferedMatrix-class}
\alias{buffer.dim}
\alias{set.buffer.dim}
\alias{ColMode}
\alias{RowMode}
\alias{is.ColMode}
\alias{is.RowMode}


\alias{dim,BufferedMatrix-method}
\alias{buffer.dim,BufferedMatrix-method}
\alias{nrow,BufferedMatrix-method}
\alias{ncol,BufferedMatrix-method}
\alias{[,BufferedMatrix-method}
\alias{[<-,BufferedMatrix-method}
\alias{show,BufferedMatrix-method}
\alias{set.buffer.dim,BufferedMatrix-method}
\alias{is.ColMode,BufferedMatrix-method}
\alias{is.RowMode,BufferedMatrix-method}
\alias{ColMode,BufferedMatrix-method}
\alias{RowMode,BufferedMatrix-method}


\title{Class BufferedMatrix}

\description{This is a class representation of a buffered matrix (of
  numeric data). In
  this case data is primarily stored outide main memory in temporary files.}
\section{Objects from the Class}{
  Objects can be created using the function \code{\link{createBufferedMatrix}}}
\section{Slots}{
  \describe{
    \item{\code{rawBufferedMatrix}:}{a pointer to an external structure
      used to access and store the matrix data.}
    }
}


\section{Methods}{
  
\describe{
  \item{ncol}{\code{signature(object = "BufferedMatrix")}: Returns the number of columns in the matrix}
  \item{nrow}{\code{signature(object = "BufferedMatrix")}: Returns the number of rows in the matrix}
  \item{dim}{\code{signature(object = "BufferedMatrix")}: Returns the
    dimensions of the matrix}
  \item{buffer.dim}{\code{signature(object = "BufferedMatrix")}: Returns
    the number of columns and the number of rows to be stored in the
    buffer}
  \item{set.buffer.dim}{\code{signature((object = "BufferedMatrix")}:
    Set the buffer size or resize it
  }
  
  
  
  \item{[}{\code{signature(object = "BufferedMatrix")}: matrix accessor}
  \item{[<-}{\code{signature(object = "BufferedMatrix")}: matrix
    replacer}
  \item{show}{\code{signature(object = "BufferedMatrix")}: prints basic
    information about the BufferedMatrix out to screen}

  \item{is.RowMode}{\code{signature(object = "BufferedMatrix")}:
    returns \code{TRUE} if the row buffer is active and \code{FALSE} otherwise.
  }
  \item{is.ColMode}{\code{signature(object = "BufferedMatrix")}:
    returns \code{TRUE} if the row buffer is inactive and \code{FALSE} otherwise.
  }
  \item{RowMode}{\code{signature(object = "BufferedMatrix")}:
    Activate the row buffer. 
  }
  \item{ColMode}{\code{signature(object = "BufferedMatrix")}:
    Deactivate the row buffer
  }

  

  
  }
  
}


\author{B. M. Bolstad \email{bmb@bmbolstad.com}}

\keyword{classes}
