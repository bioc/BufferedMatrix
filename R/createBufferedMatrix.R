## 
## file: createBufferedMatrix.R
##
## Aim: create a new BufferedMatrix
##
##
## History
## Feb 3, 2006 - Initial version
##


createBufferedMatrix <- function(rows, cols=0, bufferrows=1, buffercols=1,prefix="BM",directory=getwd()){

 
  
  tmp.externpointer<- .Call("R_bm_Create",prefix,directory,bufferrows,buffercols, PACKAGE="BufferedMatrix")
  
  .Call("R_bm_setRows",tmp.externpointer,rows, PACKAGE="BufferedMatrix")

  if (cols > 0){
    for (i in 1:cols){
      tmp.externpointer <- .Call("R_bm_AddColumn",tmp.externpointer, PACKAGE="BufferedMatrix")
    }
  }
  return(new("BufferedMatrix",rawBufferedMatrix=tmp.externpointer))
}
