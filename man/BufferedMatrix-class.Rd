\name{BufferedMatrix-class}
\docType{class}
\alias{BufferedMatrix-class}
\alias{buffer.dim}
\alias{set.buffer.dim}
\alias{ColMode}
\alias{RowMode}
\alias{is.ColMode}
\alias{is.RowMode}
\alias{prefix}
\alias{duplicate}
\alias{directory}
\alias{ewApply}
\alias{pow}

\alias{colMax}
\alias{colMin}
\alias{colSd}
\alias{colVars}
\alias{colMedians}
\alias{colRanges}
\alias{Max}
\alias{Min}
\alias{Sd}
\alias{Sum}
\alias{Var}
\alias{rowMax}
\alias{rowMin}
\alias{rowSd}
\alias{rowVars}
\alias{colApply}
\alias{rowApply}

\alias{ReadOnlyMode}
\alias{is.ReadOnlyMode}
\alias{filenames}
\alias{disk.usage}
\alias{memory.usage}


\alias{subBufferedMatrix}

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
\alias{duplicate,BufferedMatrix-method}
\alias{prefix,BufferedMatrix-method}
\alias{directory,BufferedMatrix-method}
\alias{ewApply,BufferedMatrix-method}
\alias{exp,BufferedMatrix-method}
\alias{sqrt,BufferedMatrix-method}
\alias{pow,BufferedMatrix-method}
\alias{log,BufferedMatrix-method}

\alias{colMax,BufferedMatrix-method}
\alias{rowMax,BufferedMatrix-method}
\alias{colMeans,BufferedMatrix-method}
\alias{rowMeans,BufferedMatrix-method}
\alias{colMin,BufferedMatrix-method}
\alias{rowMin,BufferedMatrix-method}
\alias{colVars,BufferedMatrix-method}
\alias{rowVars,BufferedMatrix-method}
\alias{colSd,BufferedMatrix-method}
\alias{rowSd,BufferedMatrix-method}
\alias{colSums,BufferedMatrix-method}
\alias{rowSums,BufferedMatrix-method}
\alias{colMedians,BufferedMatrix-method}
\alias{colRanges,BufferedMatrix-method}


\alias{Max,BufferedMatrix-method}
\alias{Min,BufferedMatrix-method}
\alias{Var,BufferedMatrix-method}
\alias{Sd,BufferedMatrix-method}
\alias{Sum,BufferedMatrix-method}
\alias{mean,BufferedMatrix-method}

\alias{rowApply,BufferedMatrix-method}
\alias{colApply,BufferedMatrix-method}

\alias{as.matrix,BufferedMatrix-method}
\alias{subBufferedMatrix,BufferedMatrix-method}

\alias{colnames,BufferedMatrix-method}
\alias{rownames,BufferedMatrix-method}
\alias{colnames<-,BufferedMatrix-method}
\alias{rownames<-,BufferedMatrix-method}

\alias{dimnames,BufferedMatrix-method}
\alias{dimnames<-,BufferedMatrix-method}

\alias{ReadOnlyMode,BufferedMatrix-method}
\alias{is.ReadOnlyMode,BufferedMatrix-method}
\alias{filenames,BufferedMatrix-method}
\alias{disk.usage,BufferedMatrix-method}
\alias{memory.usage,BufferedMatrix-method}

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
    \item{\code{rownames}:}{rownames for the matrix.}
    \item{\code{colnames}:}{colnames for the matrix.}
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
  \item{set.buffer.dim}{\code{signature(object = "BufferedMatrix")}:
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

  \item{duplicate}{\code{signature(object = "BufferedMatrix")}:
    Make a copy of the BufferedMatrix
  }

  \item{prefix}{\code{signature(object = "BufferedMatrix")}:
    return the initial part of the string used for temporary files
  }

  \item{directory}{\code{signature(object = "BufferedMatrix")}:
    return the location where temporary files are stored
  }

  \item{filenames}{\code{signature(object = "BufferedMatrix")}:
    return the fully pathed filenames for each column in the matrix
  }

  
  \item{ewApply}{\code{signature(object = "BufferedMatrix")}:
    apply a function elementwise
  } 

  \item{exp}{\code{signature(object = "BufferedMatrix")}: Compute the
    exponential elementwise of the matrix
  }
  
  \item{sqrt}{\code{signature(object = "BufferedMatrix")}: Compute the
    square-root elementwise of the matrix
  } 

  \item{pow}{\code{signature(object = "BufferedMatrix")}:  Compute
    $x^{power}$ elementwise of the matrix
  }

  \item{log}{\code{signature(object = "BufferedMatrix")}:  Compute
    logarithm elementwise of the matrix
  }

  \item{colMax}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing maximums by column
  }

  \item{rowMax}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing maximums by row
  }

  
  \item{colMeans}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing means by column
  }
  
  \item{rowMeans}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing means by row
  }

  \item{colMin}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing minimums by column
  }


  \item{rowMin}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing minimums by row
  }

  \item{colVars}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sample variances by column
  }

  \item{rowVars}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sample variances by row
  }

  \item{colSd}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sample standard deviations by column
  }

  \item{rowSd}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sample standard deviations by row
  }

  \item{colSums}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sum by column
  }

  \item{rowSums}{\code{signature(object = "BufferedMatrix")}: Returns a
    vector containing sum by row
  }
 
  \item{Max}{\code{signature(object = "BufferedMatrix")}: Returns the
    maximum of all elements in the matrix
  }
  
  \item{Min}{\code{signature(object = "BufferedMatrix")}: Returns the
    minimum of all elements in the matrix
  }

  \item{Var}{\code{signature(object = "BufferedMatrix")}: Returns the
    sample variance of all elements in the matrix
  }

  \item{Sd}{\code{signature(object = "BufferedMatrix")}: Returns the
    sample standard deviations of all elements in the matrix
  }
  
  \item{Sum}{\code{signature(object = "BufferedMatrix")}: Returns the
    sum of all elements in the matrix
  }

  \item{mean}{\code{signature(object = "BufferedMatrix")}: Returns the
    mean of all elements in the matrix
  }

  \item{colApply}{\code{signature(object = "BufferedMatrix")}:
    apply a function columnwise. Returns either a vector or BufferedMatrix.
  } 

  \item{rowApply}{\code{signature(object = "BufferedMatrix")}:
    apply a function row-wise. Returns either a vector or BufferedMatrix.
  } 

  \item{as.matrix}{\code{signature(object = "BufferedMatrix")}:
    coerce BufferedMatrix into a regular R \code{\link{matrix}}
  }

  \item{subBufferedMatrix}{\code{signature(object = "BufferedMatrix")}:
    gets data from BufferedMatrix and returns it in another
    BufferedMatrix
  }

  \item{rownames}{\code{signature(object = "BufferedMatrix")} : access
    the row names
  }

  \item{colnames}{\code{signature(object = "BufferedMatrix")} : access
    the column names
  }

  \item{rownames<-}{\code{signature(object = "BufferedMatrix")} : replace
    the row names
  }

  \item{colnames<-}{\code{signature(object = "BufferedMatrix")} : replace
    the column names
  }

  \item{dimnames}{\code{signature(object = "BufferedMatrix")} : Access
    the row and column names
  }

  \item{dimnames}{\code{signature(object = "BufferedMatrix")} : Replace
    the row and column names
  }


  \item{ReadOnlyMode}{\code{signature(object = "BufferedMatrix")} :
    Toggles the Read Only mode on and off
  }

  \item{is.ReadOnlyMode}{\code{signature(object = "BufferedMatrix")} :
    Finds out if it is in Read Only Mode
  }

  \item{memory.usage}{\code{signature(object = "BufferedMatrix")} :
    Give amount of RAM currently in use by BufferedMatrix object
  }

 \item{disk.usage}{\code{signature(object = "BufferedMatrix")} :
    Give amount of disk space currently in use by BufferedMatrix object
  }

  
  
  
  }}




\author{B. M. Bolstad \email{bmb@bmbolstad.com}}

\keyword{classes}
