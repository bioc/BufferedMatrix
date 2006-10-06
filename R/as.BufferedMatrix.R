##
##
## file: as.BufferedMatrix.R
##
## Aim: coerce an ordinary R matrix (or vector) to be a buffered.matrix.
## Aim: check whether object is a BufferedMatrix
## 
## Oct 3, 2006 - make as.BufferedMatrix call the C version
## Oct 6, 2006 - allow directory information to be passed into the function, allong with buffer size



as.BufferedMatrix <- function(x,bufferrows=1, buffercols=1,directory=getwd()){


  if (!(is.matrix(x) | is.vector(x))){
    stop("Can't coerce this object to BufferedMatrix")
  }


  if ((storage.mode(x) != "double") & (storage.mode(x) != "integer")){
    stop("Can only coerce numeric matrices to BufferedMatrix storage")
  }


  if (is.matrix(x)){
    newBufferedMatrix <- createBufferedMatrix(dim(x)[1],dim(x)[2],bufferrows, buffercols,directory)

    if (storage.mode(x) == "double"){

      .Call("R_bm_as_BufferedMatrix",newBufferedMatrix@rawBufferedMatrix,x,PACKAGE="BufferedMatrix")
      ##for (col in 1:dim(x)[2]){
      ##  newBufferedMatrix[,col] <- x[,col]
      ##}
    } else if (storage.mode(x) == "integer"){
      for (col in 1:dim(x)[2]){
        newBufferedMatrix[,col] <- as.double(x[,col])
      }
    }
  } else if (is.vector(x)){
    newBufferedMatrix <- createBufferedMatrix(length(x),1)

    if (storage.mode(x) == "double"){
      newBufferedMatrix[,1] <- x
    } else if (storage.mode(x) == "integer"){
      newBufferedMatrix[,1] <- as.double(x)
    }

    


  }
  newBufferedMatrix

}



is.BufferedMatrix <- function(x){


  if (class(x) == "BufferedMatrix"){
    .Call("isBufferedMatrix",x@rawBufferedMatrix,PACKAGE="BufferedMatrix");
  } else {
    return(FALSE)
  }
}
