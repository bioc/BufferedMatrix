/*****************************************************
 **
 ** file: RBufferedMatrix.c
 **
 ** Copyright (C) 2006    B. M. Bolstad
 **
 ** aim: c level R-interface for BufferedMatrix objects.
 **
 **
 **  History
 **  Feb 2, 2006 Initial version
 **
 *****************************************************/

#include "doubleBufferedMatrix.h"

#include <Rdefines.h>
#include <Rinternals.h>



SEXP R_bm_Create(SEXP Rprefix, SEXP Rdirectory, SEXP max_rows, SEXP max_cols){

  char prefix[15] = "dbmtest";
  char directory[2] = ".";

  SEXP val;

  doubleBufferedMatrix Matrix;

  Matrix = dbm_alloc(1,1,prefix,directory);



  PROTECT(val = R_MakeExternalPtr(Matrix, R_NilValue, R_NilValue));


  UNPROTECT(1);
  return val;
}



SEXP R_bm_Test_C(SEXP R_BufferedMatrix){

  int i,j;
  double temp;

  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  dbm_setRows(Matrix,5);
  
  for (i = 0; i < 5; i++){
    dbm_AddColumn(Matrix);
  }
  

  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(Matrix));
  Rprintf("Cols: %d\n",dbm_getCols(Matrix));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(Matrix));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(Matrix));

  Rprintf("\n");
  Rprintf("Assigning Values\n");
  for (i =0; i < 5; i++){
    for (j=0; j < 5; j++){
      dbm_setValue(Matrix,i,j,(double)(i+j));
    }
  }

  for (i=0; i < 5; i++){
    for (j=0; j < 5; j++){
      dbm_getValue(Matrix,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  
  return R_BufferedMatrix;
}



SEXP R_bm_Test_C2(SEXP R_BufferedMatrix){


  int i,j;
  double temp;

  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(Matrix));
  Rprintf("Cols: %d\n",dbm_getCols(Matrix));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(Matrix));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(Matrix));

  Rprintf("\n");
  Rprintf("Printing Values\n");
  for (i =0; i <dbm_getRows(Matrix); i++){
    for (j=0; j < dbm_getCols(Matrix); j++){
      dbm_getValue(Matrix,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  return R_BufferedMatrix;

}


/*

P <- .Call("R_bm_Create")
.Call("R_bm_Test_C",P)
.Call("R_bm_Test_C2",P)
*/




SEXP R_bm_Destroy(SEXP R_BufferedMatrix){

  doubleBufferedMatrix Matrix;
  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  dbm_free(Matrix);

  return R_BufferedMatrix;


}



SEXP R_bm_setRows(SEXP R_BufferedMatrix, SEXP R_rows){

  SEXP returnvalue;

  doubleBufferedMatrix Matrix;
  int rows;

  PROTECT(returnvalue=allocVector(LGLSXP,1));


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  rows = asInteger(R_rows);
    
  if(!dbm_setRows(Matrix,rows)){
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }   
  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;

}
