## 
## file: BufferedMatrix.R
##
## Aim: define the BufferedMatrix S4 class and
##      and applicable methods
##
## History
## Feb 3, 2006 - Initial version
## Feb 7, 2006 - indexing/subsetting operators for accessing and replacing parts of the matrix
## Feb 8, 2006 - show method, nrow, ncol, is.ColMode, is.RowMode, ColMode, RowMode 
## Feb 16, 2006 - duplicate method, prefix method, directory method
## Feb 17, 2006 - ewApply method
## Feb 22, 2006 - add Max, Min, mean, Sd, Var methods. Also rowMeans,rowSums,colMeans,colSums
## Feb 23, 2006 - added rowVar, rowSd, colVar, colSd, rowMax, colMax, rowMin, colMin methods

setClass("BufferedMatrix",
           representation(rawBufferedMatrix="externalptr"),
           prototype=list())




setMethod("dim", "BufferedMatrix", function(x){
          .Call("R_bm_getSize",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
          })



#Temp <- createBufferedMatrix(100)
#dim(Temp)

if(!isGeneric("buffer.dim") )
  setGeneric("buffer.dim", function(x)
             standardGeneric("buffer.dim"))


setMethod("buffer.dim", "BufferedMatrix", function(x){
          .Call("R_bm_getBufferSize",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
          })


setMethod("[", "BufferedMatrix", function(x, i, j,..., drop=FALSE) {

  ## indexing a single cell or submatrix
  if( !missing(i) & !missing(j)) {
    if ((length(i) == 1 & all(i > 0)) & (length(j) ==1 & all(j > 0))){
      ##single element
      dim.x <- dim(x)
      if ((abs(i) < 1) | (abs(i) > dim.x[1]) | (abs(j) < 1) | (abs(j) > dim.x[2])){
        stop("subscript out of bounds")
      }

      if ((i > 0) & (j >0)){
        return(.Call("R_bm_getValue",x@rawBufferedMatrix,as.integer(i-1),as.integer(j-1),PACKAGE="BufferedMatrix"))
      } else {
        ## at least one of i and j is negative
        ##
        indices.row <- (1:dim.x[1])[i]
        indices.col <- (1:dim.x[2])[j]
        return(.Call("R_bm_getValueSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),PACKAGE="BufferedMatrix"))
      }
    } else {
      ##multiple elements
      dim.x <- dim(x)
      
      if (any(abs(i) < 1) | any(abs(i) > dim.x[1]) | any(abs(j) < 1) | any(abs(j) > dim.x[2])){
        stop("subscript out of bounds")
      }
      indices.row <- (1:dim.x[1])[i]
      indices.col <- (1:dim.x[2])[j]
      return(.Call("R_bm_getValueSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),PACKAGE="BufferedMatrix"))
    }

    
  }

  ## indexing a row or set of rows
  if( !missing(i) & missing(j)) {
    dim.x <- dim(x)
    if (any(abs(i) < 1) | any(abs(i) > dim.x[1])){
      stop("subscript out of bounds")
    }
    if (any(i < 0) & !all(i <0)){
      stop("can't mix positive and negative indices")
    }
    if (all(i <0)){
      indices <- (1:dim.x[1])[i]
      return(.Call("R_bm_getValueRow",x@rawBufferedMatrix,as.integer(indices-1),PACKAGE="BufferedMatrix"))
    } else {
      return(.Call("R_bm_getValueRow",x@rawBufferedMatrix,as.integer(i-1),PACKAGE="BufferedMatrix"))
    }
  }


  ## indexing a column or set of columns
  if( !missing(j) & missing(i)) {
    dim.x <- dim(x)
    if (any(abs(j) < 1) | any(abs(j) > dim.x[2])){
      stop("subscript out of bounds")
    }
    if (any(j < 0) & !all(j <0)){
      stop("can't mix positive and negative indices")
    }
    if (all(j <0)){
      indices <- (1:dim.x[2])[j]
      return(.Call("R_bm_getValueColumn",x@rawBufferedMatrix,as.integer(indices-1),PACKAGE="BufferedMatrix"))
    } else {
      return(.Call("R_bm_getValueColumn",x@rawBufferedMatrix,as.integer(j-1),PACKAGE="BufferedMatrix"))
    }
  }

  

  return(x)
})



setReplaceMethod("[", "BufferedMatrix", function(x, i, j,..., value){


  # how many copies of value are needed to fill column-wise of size rows
  recycle.how.many.columns <- function(value,columns,rows){
    if (is.vector(value)){
      length.value <- length(value)     
    } else if (is.matrix(value)) {
      dim.value <- dim(value)
      length.value <- dim.value[1]*dim.value[2]
    }
    
    
    if (length.value < rows){
      if ((columns*rows) %% length.value == 0){
        return ((columns*rows) %/% length.value)
      } else {
        stop("number of items to replace is not a multiple of replacement length")
      }
      
    } else if (length.value > rows){
      if (length.value > rows *columns){
        stop("number of items to replace is not a multiple of replacement length")
      }
      if (length.value %% (columns*rows) == 0){
        return ((length.value) %/%(columns*rows)) 
      } else if ((rows*columns) %% length.value == 0){
        return ((columns*rows)%/%(length.value) )
      } else {
        stop("number of items to replace is not a multiple of replacement length")
      }
      
    } else {
      return ((length.value*columns) %/%(rows))
    } 
    
    
    
    
  }


  if (!is.double(value)){
    value <- as.double(value)
  }

  
  
  if( !missing(i) & !missing(j)) {
    if ((length(i) == 1) & (length(j) ==1)){
      ##single element
      dim.x <- dim(x)
      if ((abs(i) < 1) | (abs(i) > dim.x[1]) | (abs(j) < 1) | (abs(j) > dim.x[2])){
        stop("subscript out of bounds")
      }
      if (i > 0 & j > 0){
        value <- rep(value,recycle.how.many.columns(value,1,1))
        .Call("R_bm_setValue",x@rawBufferedMatrix,as.integer(i-1),as.integer(j-1),value,PACKAGE="BufferedMatrix")
      } else {
        indices.row <- (1:dim.x[1])[i]
        indices.col <- (1:dim.x[2])[j]
        value <- rep(value,recycle.how.many.columns(value,length(indices.col),length(indices.row)))
        .Call("R_bm_setValueSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),value,PACKAGE="BufferedMatrix")
      }
    }else {
      dim.x <- dim(x)
      indices.row <- (1:dim.x[1])[i]
      indices.col <- (1:dim.x[2])[j]
      value <- rep(value,recycle.how.many.columns(value,length(indices.col),length(indices.row)))
      .Call("R_bm_setValueSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),value,PACKAGE="BufferedMatrix")
    }
  }



  ## replacing a row or set of rows
  if( !missing(i) & missing(j)) {
    dim.x <- dim(x)

    if (any(abs(i) < 1) | any(abs(i) > dim.x[1])){
      stop("subscript out of bounds")
    }
    if (any(i < 0) & !all(i <0)){
      stop("can't mix positive and negative indices")
    }

    if (all(i < 0)){
      indices <- (1:dim.x[1])[i]
      value <- rep(value,recycle.how.many.columns(value,dim.x[2],length(indices)))
      .Call("R_bm_setValueRow",x@rawBufferedMatrix,as.integer(indices-1),value,PACKAGE="BufferedMatrix")
    } else {
      value <- rep(value,recycle.how.many.columns(value,dim.x[2],length(i)))
      .Call("R_bm_setValueRow",x@rawBufferedMatrix,as.integer(i-1),value,PACKAGE="BufferedMatrix")
    }

    
  }


  ## replacing a column or set of columns
  if( !missing(j) & missing(i)) {
    dim.x <- dim(x)
    if (any(abs(j) < 1) | any(abs(j) > dim.x[2])){
      stop("subscript out of bounds")
    }
    if (any(j < 0) & !all(j <0)){
      stop("can't mix positive and negative indices")
    }
    if (all(j <0)){
      indices <- (1:dim.x[2])[j]
      value <- rep(value,recycle.how.many.columns(value,length(indices),dim.x[1]))
      .Call("R_bm_setValueColumn",x@rawBufferedMatrix,as.integer(indices-1),value,PACKAGE="BufferedMatrix")
    } else {
      value <- rep(value,recycle.how.many.columns(value,length(j),dim.x[1]))
      .Call("R_bm_setValueColumn",x@rawBufferedMatrix,as.integer(j-1),value,PACKAGE="BufferedMatrix")
    }
  }
  return(x)
  

})




setMethod("show", "BufferedMatrix", function(object){

  cat("BufferedMatrix object\n")
  cat("Matrix size: ",dim(object),"\n")
  cat("Buffer size: ",buffer.dim(object),"\n")
  cat("Directory:   ",directory(object),"\n")
  cat("Prefix:      ",prefix(object),"\n")
  cat("Mode: ");
  if (is.RowMode(object)){
    cat("Row mode\n")
  } else {
    cat("Col mode\n")
  }
  
  
  
})



setMethod("ncol","BufferedMatrix",function(x){
  dim(x)[2]
})


setMethod("nrow","BufferedMatrix",function(x){
  dim(x)[1]
})



if(!isGeneric("is.ColMode") )
  setGeneric("is.ColMode", function(x)
             standardGeneric("is.ColMode"))



setMethod("is.ColMode","BufferedMatrix",function(x){

  return(!.Call("R_bm_isRowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})



if(!isGeneric("is.RowMode") )
  setGeneric("is.RowMode", function(x)
             standardGeneric("is.RowMode"))



setMethod("is.RowMode","BufferedMatrix",function(x){

  return(.Call("R_bm_isRowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})


if(!isGeneric("RowMode"))
  setGeneric("RowMode", function(x)
             standardGeneric("RowMode"))
  


setMethod("RowMode","BufferedMatrix",function(x){
  return(.Call("R_bm_RowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})



if(!isGeneric("ColMode"))
  setGeneric("ColMode", function(x)
             standardGeneric("ColMode"))
  

setMethod("ColMode","BufferedMatrix",function(x){
  return(.Call("R_bm_ColMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})



if(!isGeneric("set.buffer.dim") )
  setGeneric("set.buffer.dim", function(x,rows,cols)
             standardGeneric("set.buffer.dim"))


setMethod("set.buffer.dim", "BufferedMatrix", function(x,rows,cols){
          .Call("R_bm_ResizeBuffer",x@rawBufferedMatrix,rows,cols,PACKAGE="BufferedMatrix")
          })



if (!isGeneric("prefix"))
  setGeneric("prefix",function(x)
             standardGeneric("prefix"))


setMethod("prefix","BufferedMatrix",function(x){
  .Call("R_bm_getPrefix",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

  
})


if (!isGeneric("directory"))
  setGeneric("directory",function(x)
             standardGeneric("directory"))


setMethod("directory","BufferedMatrix",function(x){
  .Call("R_bm_getDirectory",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

  
})



if(!isGeneric("duplicate") )
  setGeneric("duplicate", function(x,...)
             standardGeneric("duplicate"))



setMethod("duplicate", "BufferedMatrix", function(x,prefix="BM",dir) {

  bufferrows <- buffer.dim(x)[1]
  buffercols <- buffer.dim(x)[2]
  rows <- nrow(x)
  cols <- ncol(x)
  

  
  if (missing(dir)){
    my.directory <- directory(x)
  } else {
    my.directory <- dir
  }

  if (missing(prefix)){
    my.prefix <- prefix(x)
  } else {
    my.prefix <-prefix
  }

  
  
  tmp.externpointer<- .Call("R_bm_Create",my.prefix,my.directory,bufferrows,buffercols, PACKAGE="BufferedMatrix")

  .Call("R_bm_setRows",tmp.externpointer,rows, PACKAGE="BufferedMatrix")

  if (cols > 0){
    for (i in 1:cols){
      .Call("R_bm_AddColumn",tmp.externpointer, PACKAGE="BufferedMatrix")
    }
  }
  if(!.Call("R_bm_copyValues",tmp.externpointer, x@rawBufferedMatrix, PACKAGE="BufferedMatrix")){
    stop("Duplication failed in data copying stage\n");
  }
  
  y <- new("BufferedMatrix",rawBufferedMatrix=tmp.externpointer)

  return(y)

  
})



if(!isGeneric("ewApply") )
  setGeneric("ewApply", function(x,...)
             standardGeneric("ewApply"))


setMethod("ewApply", "BufferedMatrix", function(x,FUN,...){

  if (missing(FUN)){
    stop("Must provide function to apply.")
  }

  FUN <- match.fun(FUN)


  fc <- function(x) {
    x <- FUN(x,...)
    if (!is.numeric(x)) stop("Need numeric result")
    as.double(x)
  }
  
  
  if(!.Call("R_bm_ewApply", x@rawBufferedMatrix,body(fc), new.env(), PACKAGE="BufferedMatrix")){
    stop("Problem applying function elementwise")
  }
  
  return(x)
})





setMethod("sqrt","BufferedMatrix",function(x){

  
  return(.Call("R_bm_ewSqrt",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))
  


})


setMethod("exp","BufferedMatrix",function(x){

  
  return(.Call("R_bm_ewExp",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))
  


})


setMethod("log","BufferedMatrix",function(x,base = exp(1)){



  
  return(.Call("R_bm_ewLog",x@rawBufferedMatrix,base,PACKAGE="BufferedMatrix"))
  


})



if(!isGeneric("pow") )
  setGeneric("pow", function(x,...)
             standardGeneric("pow"))



setMethod("pow","BufferedMatrix",function(x,power=1){



  
  return(.Call("R_bm_ewPow",x@rawBufferedMatrix,power,PACKAGE="BufferedMatrix"))
  


})



if(!isGeneric("Max") )
  setGeneric("Max", function(x,...)
             standardGeneric("Max"))



setMethod("Max", signature("BufferedMatrix","logical"), function(x,na.rm=FALSE){
  return(.Call("R_bm_max",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})




if(!isGeneric("Min") )
  setGeneric("Min", function(x,...)
             standardGeneric("Min"))


setMethod("Min", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_min",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("mean", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_mean",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


if(!isGeneric("Sum") )
  setGeneric("Sum", function(x,...)
             standardGeneric("Sum"))


setMethod("Sum","BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_sum",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


if(!isGeneric("Var") )
  setGeneric("Var", function(x,...)
             standardGeneric("Var"))


setMethod("Var", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_var",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


if(!isGeneric("Sd") )
  setGeneric("Sd", function(x,...)
             standardGeneric("Sd"))


setMethod("Sd", "BufferedMatrix", function(x,na.rm=FALSE){
  sqrt(Var(x,na.rm))
})




if(!isGeneric("rowMeans") )
  setGeneric("rowMeans", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowMeans"))

setMethod("rowMeans","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMeans",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})



if(!isGeneric("rowSums") )
  setGeneric("rowSums", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowSums"))



setMethod("rowSums","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowSums",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


if(!isGeneric("rowVars") )
  setGeneric("rowVars", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowVars"))


setMethod("rowVars","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowVars",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})

if(!isGeneric("rowSd") )
  setGeneric("rowSd", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowSd"))


setMethod("rowSd","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(sqrt(rowVars(x,na.rm,dims)))


})



if(!isGeneric("rowMax") )
  setGeneric("rowMax", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowMax"))


setMethod("rowMax","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMax",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})




if(!isGeneric("rowMin") )
  setGeneric("rowMin", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("rowMin"))


setMethod("rowMin","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMin",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})




if(!isGeneric("colMeans") )
  setGeneric("colMeans", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colMeans"))

setMethod("colMeans","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMeans",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})





if(!isGeneric("colSums") )
  setGeneric("colSums", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colSums"))



setMethod("colSums","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colSums",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


if(!isGeneric("colVars") )
  setGeneric("colVars", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colVars"))


setMethod("colVars","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colVars",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


if(!isGeneric("colSd") )
  setGeneric("colSd", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colSd"))


setMethod("colSd","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(sqrt(colVars(x,na.rm,dims)))


})




if(!isGeneric("colMax") )
  setGeneric("colMax", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colMax"))


setMethod("colMax","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMax",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})





if(!isGeneric("colMin") )
  setGeneric("colMin", function(x,na.rm = FALSE, dims = 1)
             standardGeneric("colMin"))


setMethod("colMin","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMin",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})
