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
## Apr 26, 2006 - add colApply
## May 30, 2006 - add subBufferedMatrix
## June 9, 2006 - add rownames and colnames with accessors and replacement functions
##                also allow some subsetting/indexing using these.
## June 12, 2006 - add dimnames accessors and replacement functions
## June 29, 2006 - add ReadOnly and is.ReadOnly (which inexplicably had the C code done
##                 but were never had methods implemented)
## Oct 21, 2006  - add colMedians
## Oct 22, 2006  - add colRanges
## Oct 27, 2006  - add filenames method, memory.usage method
## Jan 4, 2007   - remove isGeneric/setGeneric idiom. setGeneric's have been moved to their own file

setClass("BufferedMatrix",
           representation(rawBufferedMatrix="externalptr",rownames="character",colnames="character"),
           prototype=list(rownames=character(0),colnames=character(0)))




setMethod("dim", "BufferedMatrix", function(x){
          .Call("R_bm_getSize",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
          })



setMethod("buffer.dim", "BufferedMatrix", function(x){
          .Call("R_bm_getBufferSize",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
          })


setMethod("[", "BufferedMatrix", function(x, i, j,..., drop=FALSE) {


  if (!missing(i)){
    if (is.character(i)){
      which.i <- match(i,x@rownames)
      which.i <- which.i[!is.na(which.i)]
      if (length(which.i) == 0){
        stop("subscript out of bounds")
      } else {
        i <- which.i
      }
    } else if (is.logical(i)){
      if (length(i) != dim(x)[1]){
        stop("Incorrect length logical indexing vector should match number of rows.")
      }
      i <- which(i)
    }
  }

  
  if (!missing(j)){
    if (is.character(j)){
      which.j <- match(j,x@colnames)
      which.j <- which.j[!is.na(which.j)]
      if (length(which.j) == 0){
        stop("subscript out of bounds")
      } else {
        j <- which.j
      }
    } else if (is.logical(j)){
      if (length(j) != dim(x)[2]){
        stop("Incorrect length logical indexing vector should match number of columns.")
      }
      j <- which(j)
    }
  }

  


  
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
      result <- .Call("R_bm_getValueSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),PACKAGE="BufferedMatrix")
      rownames(result) <- rownames(x)[indices.row]
      colnames(result) <- colnames(x)[indices.col]
      return(result)
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

      result <- .Call("R_bm_getValueRow",x@rawBufferedMatrix,as.integer(indices-1),PACKAGE="BufferedMatrix")
      colnames(result) <- colnames(x)
      rownames(result) <- rownames(x)[indices]
      return(result)
    } else {
      result <- .Call("R_bm_getValueRow",x@rawBufferedMatrix,as.integer(i-1),PACKAGE="BufferedMatrix")
      colnames(result) <- colnames(x)
      rownames(result) <- rownames(x)[i]
      return(result)
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
      result <- .Call("R_bm_getValueColumn",x@rawBufferedMatrix,as.integer(indices-1),PACKAGE="BufferedMatrix")
      rownames(result) <- rownames(x)
      colnames(result) <- colnames(x)[indices]
      return(result)
    } else {
      result <-.Call("R_bm_getValueColumn",x@rawBufferedMatrix,as.integer(j-1),PACKAGE="BufferedMatrix")
      rownames(result) <- rownames(x)
      colnames(result) <- colnames(x)[j]
      return(result)
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

  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }


  if (!is.double(value)){
    value <- as.double(value)
  }

  
  if (!missing(i)){
    if (is.character(i)){
      which.i <- match(i,x@rownames)
      which.i <- which.i[!is.na(which.i)]
      if (length(which.i) == 0){
        stop("subscript out of bounds")
      } else {
        i <- which.i
      }
    } else if (is.logical(i)){
      if (length(i) != dim(x)[1]){
        stop("Incorrect length logical indexing vector should match number of rows.")
      }
      i <- which(i)
    }
  }

  
  if (!missing(j)){
    if (is.character(j)){
      which.j <- match(j,x@colnames)
      which.j <- which.j[!is.na(which.j)]
      if (length(which.j) == 0){
        stop("subscript out of bounds")
      } else {
        j <- which.j
      }
    } else if (is.logical(j)){
      if (length(j) != dim(x)[2]){
        stop("Incorrect length logical indexing vector should match number of columns.")
      }
      j <- which(j)
    }
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
  cat("Read Only: ")
  cat(is.ReadOnlyMode(object))
  cat("\n")

  mem.usage <- memory.usage(object)

  if (mem.usage < 1024){
    cat("Memory usage : ",mem.usage," bytes.\n")
  } else if (mem.usage < 1024^2) {
    cat("Memory usage : ",round(mem.usage/1024,1)," Kilobytes.\n")
  } else {
    cat("Memory usage : ",round(mem.usage/1024^2,2)," Megabytes.\n")
  }
  
  disk.space.used <- disk.usage(object)
  if (disk.space.used < 1024){
    cat("Disk usage : ",disk.space.used," bytes.\n")
  } else if (mem.usage < 1024^2) {
    cat("Disk usage : ",round(disk.space.used/1024,1)," Kilobytes.\n")
  } else {
    cat("Disk usage : ",round(disk.space.used/1024^2,2)," Megabytes.\n")
  }

  


  
})



setMethod("ncol","BufferedMatrix",function(x){
  dim(x)[2]
})


setMethod("nrow","BufferedMatrix",function(x){
  dim(x)[1]
})




setMethod("is.ColMode","BufferedMatrix",function(x){

  return(!.Call("R_bm_isRowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})


setMethod("is.RowMode","BufferedMatrix",function(x){

  return(.Call("R_bm_isRowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})



setMethod("RowMode","BufferedMatrix",function(x){
  return(.Call("R_bm_RowMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})



setMethod("ColMode","BufferedMatrix",function(x){
  return(.Call("R_bm_ColMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix"))

})





setMethod("set.buffer.dim", "BufferedMatrix", function(x,rows,cols){
          .Call("R_bm_ResizeBuffer",x@rawBufferedMatrix,rows,cols,PACKAGE="BufferedMatrix")
          })



setMethod("prefix","BufferedMatrix",function(x){
  .Call("R_bm_getPrefix",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

  
})



setMethod("directory","BufferedMatrix",function(x){
  .Call("R_bm_getDirectory",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

  
})




setMethod("filenames","BufferedMatrix",function(x){
  .Call("R_bm_getFileNames",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
})




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


setMethod("ewApply", "BufferedMatrix", function(x,FUN,...){


  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }
  

  
  if (missing(FUN)){
    stop("Must provide function to apply.")
  }

  FUN <- match.fun(FUN)


  fc <- function(x) {
    x <- FUN(x,...)
    if (!is.numeric(x)) stop("Need numeric result")
    as.double(x)
  }
  

  check.function.return.length <- function(){
    if (length(fc(c(1))) != 1){
      return(FALSE)
    }
    if (length(fc(c(1,2,3))) != 3){
      return(FALSE)
    }
    return(TRUE)
  }
  

  if (!check.function.return.length()){
    stop("This function should return a vector of same length as input. Cannot apply elementwise")

  }
  
  if(!.Call("R_bm_ewApply", x@rawBufferedMatrix,body(fc), new.env(), PACKAGE="BufferedMatrix")){
    stop("Problem applying function elementwise")
  }
  
  return(x)
})





setMethod("sqrt","BufferedMatrix",function(x){

  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }

  return(invisible(.Call("R_bm_ewSqrt",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")))
  
})


setMethod("exp","BufferedMatrix",function(x){

  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }
  
  
  return(invisible(.Call("R_bm_ewExp",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")))
})


setMethod("log","BufferedMatrix",function(x,base = exp(1)){

  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }
  
  return(invisible(.Call("R_bm_ewLog",x@rawBufferedMatrix,base,PACKAGE="BufferedMatrix")))
})



setMethod("pow","BufferedMatrix",function(x,power=1){

  
  if (is.ReadOnlyMode(x)){
    stop("BufferedMatrix is ReadOnly.")
  }

  return(invisible(.Call("R_bm_ewPow",x@rawBufferedMatrix,power,PACKAGE="BufferedMatrix")))
})


setMethod("Max", signature("BufferedMatrix"), function(x,na.rm=FALSE){
  return(.Call("R_bm_max",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("Min", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_min",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("mean", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_mean",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("Sum","BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_sum",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("Var", "BufferedMatrix", function(x,na.rm=FALSE){
  return(.Call("R_bm_var",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))
})


setMethod("Sd", "BufferedMatrix", function(x,na.rm=FALSE){
  sqrt(Var(x,na.rm))
})


setMethod("rowMeans","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMeans",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})



setMethod("rowSums","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowSums",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


setMethod("rowVars","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowVars",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


setMethod("rowSd","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(sqrt(rowVars(x,na.rm,dims)))


})



setMethod("rowMax","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMax",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})



setMethod("rowMin","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_rowMin",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))

})




setMethod("colMeans","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMeans",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))

})



setMethod("colSums","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colSums",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


setMethod("colVars","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colVars",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))


})


setMethod("colSd","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(sqrt(colVars(x,na.rm,dims)))


})



setMethod("colMax","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMax",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})




setMethod("colMin","BufferedMatrix",function(x,na.rm=FALSE,dims=1){

  return(.Call("R_bm_colMin",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})


setMethod("colMedians","BufferedMatrix",function(x,na.rm=FALSE){

  return(.Call("R_bm_colMedians",x@rawBufferedMatrix,na.rm,PACKAGE="BufferedMatrix"))



})

setMethod("colRanges","BufferedMatrix",function(x,na.rm=FALSE){

  return(.Call("R_bm_colRanges",x@rawBufferedMatrix,na.rm,FALSE,PACKAGE="BufferedMatrix"))



})



setMethod("colApply", "BufferedMatrix", function(x,FUN,...){

  if (missing(FUN)){
    stop("Must provide function to apply.")
  }

  FUN <- match.fun(FUN)


  fc <- function(x) {
    x <- FUN(x,...)
    if (!is.numeric(x)) stop("Need numeric result")
    as.double(x)
  }
  
  check.function.return.length <- function(){
    length(fc(x[,1]))
  }

  return.dim1 <- check.function.return.length()

  result <- .Call("R_bm_colApply", x@rawBufferedMatrix,return.dim1,body(fc), new.env(), PACKAGE="BufferedMatrix")

  if(!result[[1]]){
    stop("Problem applying function column wise")
  }
  if (return.dim1 ==1){
    return(do.call("c",result[[2]]))
  } else {
    return(new("BufferedMatrix",rawBufferedMatrix=result[[2]]))
  }
})




setMethod("rowApply", "BufferedMatrix", function(x,FUN,...){

  if (missing(FUN)){
    stop("Must provide function to apply.")
  }

  FUN <- match.fun(FUN)


  fc <- function(x) {
    x <- FUN(x,...)
    if (!is.numeric(x)) stop("Need numeric result")
    as.double(x)
  }
  
  check.function.return.length <- function(){
    length(fc(x[1,]))
  }

  return.dim1 <- check.function.return.length()

  result <- .Call("R_bm_rowApply", x@rawBufferedMatrix,return.dim1,body(fc), new.env(), PACKAGE="BufferedMatrix")

  if(!result[[1]]){
    stop("Problem applying function row wise")
  }
  if (return.dim1 ==1){
    return(do.call("c",result[[2]]))
  } else {
    return(new("BufferedMatrix",rawBufferedMatrix=result[[2]]))
  }
})


setMethod("as.matrix", "BufferedMatrix", function(x, ...){
  
  
  .Call("R_bm_as_matrix", x@rawBufferedMatrix, PACKAGE="BufferedMatrix")
  

})





setMethod("subBufferedMatrix","BufferedMatrix", function(x,i, j){


  ###
  ### i and j index rows and columns respectively
  ###
  ### Note this finds this data from the supplied buffered matrix
  ### and copies it into a new buffered matrix which it returns
  ###
  
  if (!missing(i)){
    if (is.character(i)){
      which.i <- match(i,x@rownames)
      which.i <- which.i[!is.na(which.i)]
      if (length(which.i) == 0){
        stop("subscript out of bounds")
      } else {
        i <- which.i
      }
    } else if (is.logical(i)){
      if (length(i)!= dim(x)[1]){
        stop("Incorrect length logical indexing vector should match number of columns.")
      }
      i <- which(i)
    }
  }

  
  if (!missing(j)){
    if (is.character(j)){
      which.j <- match(j,x@colnames)
      which.j <- which.j[!is.na(which.j)]
      if (length(which.j) == 0){
        stop("subscript out of bounds")
      } else {
        j <- which.j
      }
    } else if (is.logical(j)){
      if (length(j) != dim(x)[2]){
        stop("Incorrect length logical indexing vector should match number of columns.")
      }
      j <- which(j)
    }
  }

  
    ## indexing a single cell or submatrix
  if( !missing(i) & !missing(j)) {
    if ((length(i) == 1 & all(i > 0)) & (length(j) ==1 & all(j > 0))){
      ##single element
      dim.x <- dim(x)
      if ((abs(i) < 1) | (abs(i) > dim.x[1]) | (abs(j) < 1) | (abs(j) > dim.x[2])){
        stop("subscript out of bounds")
      }

      if ((i > 0) & (j >0)){
        if (!is.null(colnames(x))){
          this.colnames <-colnames(x)[i]
        } else {
          this.colnames <- character()
        }

        if (!is.null(rownames(x))){
          this.rownames <-rownames(x)[j]
        } else {
          this.rownames <- character()
        }
      
        return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(i-1),as.integer(j-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
      } else {
        ## at least one of i and j is negative
        ##
        indices.row <- (1:dim.x[1])[i]
        indices.col <- (1:dim.x[2])[j]

        if (!is.null(colnames(x))){
          this.colnames <-colnames(x)[indices.col]
        } else {
          this.colnames <- character()
        }

        if (!is.null(rownames(x))){
          this.rownames <-rownames(x)[indices.row]
        } else {
          this.rownames <- character()
        }
      
 
        return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
      }
    } else {
      ##multiple elements
      dim.x <- dim(x)
      
      if (any(abs(i) < 1) | any(abs(i) > dim.x[1]) | any(abs(j) < 1) | any(abs(j) > dim.x[2])){
        stop("subscript out of bounds")
      }
      indices.row <- (1:dim.x[1])[i]
      indices.col <- (1:dim.x[2])[j]
     

      if (!is.null(colnames(x))){
        this.colnames <-colnames(x)[indices.col]
      } else {
        this.colnames <- character()
      }

      if (!is.null(rownames(x))){
        this.rownames <-rownames(x)[indices.row]
      } else {
        this.rownames <- character()
      }
      
      return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(indices.row-1),as.integer(indices.col-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
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

      
      if (!is.null(colnames(x))){
        this.colnames <-colnames(x)
      } else {
        this.colnames <- character()
      }

      if (!is.null(rownames(x))){
        this.rownames <-rownames(x)[indices]
      } else {
        this.rownames <- character()
      }
            
      return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(indices-1),as.integer(1:dim.x[2]-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
    } else {

      if (!is.null(colnames(x))){
        this.colnames <-colnames(x)
      } else {
        this.colnames <- character()
      }

      if (!is.null(rownames(x))){
        this.rownames <-rownames(x)[i]
      } else {
        this.rownames <- character()
      }
      

      
      return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(i-1),as.integer(1:dim.x[2]-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
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

      if (!is.null(colnames(x))){
        this.colnames <-colnames(x)[indices]
      } else {
        this.colnames <- character()
      }

      if (!is.null(rownames(x))){
        this.rownames <-rownames(x)
      } else {
        this.rownames <- character()
      }
      
      return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(1:dim.x[1]-1),as.integer(indices-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
    } else {

      if (!is.null(colnames(x))){
        this.colnames <-colnames(x)[j]
      } else {
        this.colnames <- character()
      }

      if (!is.null(rownames(x))){
        this.rownames <-rownames(x)
      } else {
        this.rownames <- character()
      }
      
      
      return(new("BufferedMatrix",rawBufferedMatrix=.Call("R_bm_MakeSubmatrix",x@rawBufferedMatrix,as.integer(1:dim.x[1]-1),as.integer(j-1),PACKAGE="BufferedMatrix"),rownames=this.rownames,colnames= this.colnames))
    }
  }


  

  
})





setMethod("rownames","BufferedMatrix",function(x,do.NULL=TRUE,prefix="row"){

  if (do.NULL)
    if (length(x@rownames) == 0){
      return(NULL)
    } else {
      return(x@rownames)
    }
  else
    paste(prefix, seq(length = nrow(x)), sep = "")
  
})


setMethod("colnames","BufferedMatrix",function(x,do.NULL=TRUE,prefix="col"){

  if (do.NULL)
    if (length(x@colnames) == 0){
      return(NULL)
    } else {
      return(x@colnames)
    }
  else
    paste(prefix, seq(length = ncol(x)), sep = "")
  
})




setReplaceMethod("colnames", "BufferedMatrix",function(x,value){

  if(!is.null(value)){
    value <- as.character(value)
    value <- as.character(value)

    if (ncol(x) == length(value)){
      x@colnames <- as.vector(value)
      x
    }
    else
      stop("Incorrect length for colnames")
  } else {
    x@colnames <- character()
    x
  }
})





setReplaceMethod("rownames", "BufferedMatrix",function(x,value){

  if(!is.null(value)){
    value <- as.character(value)

    if (nrow(x) == length(value)){
      x@rownames <- as.vector(value)
      x
    }
    else
      stop("Incorrect length for rownames")
  } else {
    x@rownames <- character()
    x

  }
})





setMethod("dimnames","BufferedMatrix",function(x){


  row.names <- rownames(x)
  col.names <- colnames(x)

  if (is.null(row.names) & is.null(col.names)){
    return(NULL)
  } else {
    return(list(row.names,col.names))

  }

  


})




setReplaceMethod("dimnames","BufferedMatrix",function(x,value){


  if (is.null(value)){
    rownames(x) <- NULL
    colnames(x) <- NULL
    return(x)
  } else  if (is.list(value) & (length(value) ==2)){
    rownames(x) <- value[[1]]
    colnames(x) <- value[[2]]
    return (x)
  } else {
    stop("Wrong length supplied to repalce dimnames")
  }

})




setMethod("ReadOnlyMode","BufferedMatrix",function(x){


  .Call("R_bm_ReadOnlyModeToggle",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

})



setMethod("is.ReadOnlyMode","BufferedMatrix",function(x){


  .Call("R_bm_isReadOnlyMode",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")

})



setMethod("memory.usage","BufferedMatrix",function(x){

  .Call("R_bm_memoryInUse",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
})



setMethod("disk.usage","BufferedMatrix",function(x){

  .Call("R_bm_fileSpaceInUse",x@rawBufferedMatrix,PACKAGE="BufferedMatrix")
})



setAs("BufferedMatrix", "matrix", function(from){
  as.matrix(from)
})


setAs("matrix", "BufferedMatrix", function(from){
  as.BufferedMatrix(from)
})



setMethod("AddColumn","BufferedMatrix",function(x){
   x@rawBufferedMatrix <- .Call("R_bm_AddColumn",x@rawBufferedMatrix, PACKAGE="BufferedMatrix")
   x
})
