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


/*****************************************************
 **
 ** SEXP R_bm_Create(SEXP R_prefix, SEXP R_directory, SEXP R_max_rows, SEXP R_max_cols)
 **
 ** SEXP R_prefix - a character string to be used for start of any temporary files created
 ** SEXP R_directory - a character string giving the path where temporary files should be stored
 ** SEXP R_max_rows, R_max_cols - buffer size 
 **
 ** Creates a Buffered Matrix object and returns a pointer to
 ** it. Note that initially the matrix is empty.
 **  The number of rows must be set and columns added before 
 ** the matrix is really of any use.
 **
 ** Returns a pointer to the object
 **
 *****************************************************/


SEXP R_bm_Create(SEXP R_prefix, SEXP R_directory, SEXP R_max_rows, SEXP R_max_cols){

  char *prefix = CHAR(VECTOR_ELT(R_prefix,0));
  char *directory = CHAR(VECTOR_ELT(R_directory,0));

  double max_rows = asReal(R_max_rows);
  double max_cols = asReal(R_max_cols);

  SEXP val;

  doubleBufferedMatrix Matrix;

  Matrix = dbm_alloc(max_rows,max_cols,prefix,directory);

  PROTECT(val = R_MakeExternalPtr(Matrix, R_NilValue, R_NilValue));
  UNPROTECT(1);
  return val;
}



SEXP R_bm_Test_C(SEXP R_BufferedMatrix){

  int i,j;
  double temp;

  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }


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
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }

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



/*****************************************************
 **
 ** SEXP R_bm_Destroy(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** Deallocs a BufferedMatrix
 **
 ** Should return a null pointer
 **
 **
 *****************************************************/

SEXP R_bm_Destroy(SEXP R_BufferedMatrix){

  doubleBufferedMatrix Matrix;
  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix != NULL){
    dbm_free(Matrix);
  }
  
  
  R_BufferedMatrix = R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);

  return R_BufferedMatrix;


}

/*****************************************************
 **
 ** SEXP R_bm_setRows(SEXP R_BufferedMatrix, SEXP R_rows)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_rows - number of rows in the matrix
 **
 ** Sets the number of rows in the matrix. Once this is set
 ** it can not be altered.
 **
 ** RETURNS TRUE if operation was successful
 **         FALSE if operation did not succeed.
 **
 *****************************************************/

SEXP R_bm_setRows(SEXP R_BufferedMatrix, SEXP R_rows){

  SEXP returnvalue;

  doubleBufferedMatrix Matrix;
  int rows;

  PROTECT(returnvalue=allocVector(LGLSXP,1));


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
 
  if (Matrix == NULL){
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }

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


/*****************************************************
 **
 ** SEXP R_bm_AddColumn(SEXP R_BufferedMatrix)
 ** 
 ** SEXP R_BufferedMatrix
 **
 ** Adds an additional column to the matrix. Note the currently
 ** columns may only be added not removed
 **
 ** RETURNS pointer to the BufferedMatrix
 ** 
 **
 *****************************************************/

SEXP R_bm_AddColumn(SEXP R_BufferedMatrix){


  doubleBufferedMatrix Matrix;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }

  
  dbm_AddColumn(Matrix);

  return R_BufferedMatrix;
  

}

/*****************************************************
 **
 ** SEXP R_bm_ResizeBuffer(SEXP R_BufferedMatrix, SEXP R_new_maxrow, SEXP R_new_maxcol)
 ** 
 ** SEXP R_BufferedMatrix
 ** SEXP R_new_maxrow,R_new_maxcol - buffer dimensions
 **
 ** Resizes the buffers
 **
 ** RETURNS a pointer to the BufferedMatrix
 **
 *****************************************************/

SEXP R_bm_ResizeBuffer(SEXP R_BufferedMatrix, SEXP R_new_maxrow, SEXP R_new_maxcol){
  
  doubleBufferedMatrix Matrix;
  int new_maxrow, new_maxcol;

    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }

  new_maxrow = asInteger(R_new_maxrow);
  new_maxcol = asInteger(R_new_maxcol);

  dbm_ResizeBuffer(Matrix, new_maxrow, new_maxcol);
  return R_BufferedMatrix;
}

/*****************************************************
 **
 ** SEXP R_bm_RowMode(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** turns the row buffer on
 **
 ** RETURNS a pointer to the BufferedMatrix
 **
 *****************************************************/

SEXP R_bm_RowMode(SEXP R_BufferedMatrix){
  
  doubleBufferedMatrix Matrix;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }
  dbm_RowMode(Matrix);
  
  return R_BufferedMatrix;

}


/*****************************************************
 **
 ** SEXP R_bm_ColMode(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** turns the row buffer off
 **
 ** RETURNS a pointer to the BufferedMatrix
 **
 *****************************************************/

SEXP R_bm_ColMode(SEXP R_BufferedMatrix){
  
  doubleBufferedMatrix Matrix;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }
  dbm_ColMode(Matrix);
  
  return R_BufferedMatrix;

}

/*****************************************************
 **
 ** SEXP R_bm_SetPrefix(SEXP R_BufferedMatrix, SEXP R_Prefix)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_Prefix - string to use for the prefix
 **
 ** Sets the prefix (ie string at the beginning) of any temporary 
 ** files created. Note that changing the prefix has no effect on
 ** temporary files already created
 **
 ** RETURNS a pointer to the BufferedMatrix
 **
 *****************************************************/


SEXP R_bm_SetPrefix(SEXP R_BufferedMatrix, SEXP R_Prefix){

  
  doubleBufferedMatrix Matrix;
  char *prefix = CHAR(VECTOR_ELT(R_Prefix,0));


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }
  
  dbm_SetPrefix(Matrix,prefix);
  

  return R_BufferedMatrix;
}

/*****************************************************
 **
 ** SEXP R_bm_ReadOnlyModeToggle(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** Changes the Mode from or to ReadOnly mode.
 ** It flips the state from one to the other
 **
 ** RETURNS a pointer to the BufferedMatrix
 **
 **
 *****************************************************/

SEXP R_bm_ReadOnlyModeToggle(SEXP R_BufferedMatrix){


  doubleBufferedMatrix Matrix;
  int current_mode;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }
  
  current_mode = dbm_isReadOnlyMode(Matrix);

  dbm_ReadOnlyMode(Matrix,!current_mode);
    
  return R_BufferedMatrix;
}

/*****************************************************
 **
 ** SEXP R_bm_isReadOnlyMode(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** RETURNS TRUE if in ReadOnlyMode (ie can't set values)
 **         FALSE otherwise
 **
 **
 *****************************************************/



SEXP R_bm_isReadOnlyMode(SEXP R_BufferedMatrix){

  SEXP returnvalue;

  doubleBufferedMatrix Matrix;
  int current_mode;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    PROTECT(returnvalue=allocVector(LGLSXP,1));
    LOGICAL(returnvalue)[0] = current_mode;
    UNPROTECT(1);
    return returnvalue;
  }
  
  current_mode = dbm_isReadOnlyMode(Matrix);
  
  

  PROTECT(returnvalue=allocVector(LGLSXP,1));

  LOGICAL(returnvalue)[0] = current_mode;
  UNPROTECT(1);
  return returnvalue;
}

/*****************************************************
 **
 ** SEXP R_bm_isRowMode(SEXP R_BufferedMatrix)
 ** 
 ** SEXP R_BufferedMatrix
 ** 
 ** RETURNS TRUE if in RowMode (ie RowBuffer has been activated)
 **         FALSE otherwise
 **
 *****************************************************/

SEXP R_bm_isRowMode(SEXP R_BufferedMatrix){

  SEXP returnvalue;

  doubleBufferedMatrix Matrix;
  int current_mode;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    PROTECT(returnvalue=allocVector(LGLSXP,1));
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }
  
  current_mode = dbm_isRowMode(Matrix);
  
  

  PROTECT(returnvalue=allocVector(LGLSXP,1));

  LOGICAL(returnvalue)[0] = current_mode;
  UNPROTECT(1);
  return returnvalue;
}

/*****************************************************
 **
 ** SEXP R_bm_getSize(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** RETURNS the matrix dimensions as a vector of integers first
 **         element is rows, second element is columns
 **
 *****************************************************/

SEXP R_bm_getSize(SEXP R_BufferedMatrix){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  
  PROTECT(returnvalue=allocVector(INTSXP,2));

  if (Matrix == NULL){ 

    INTEGER(returnvalue)[0] = 0;
    INTEGER(returnvalue)[1] = 0;
    UNPROTECT(1);
    return returnvalue;
  }
  
  INTEGER(returnvalue)[0] = dbm_getRows(Matrix);
  INTEGER(returnvalue)[1] = dbm_getCols(Matrix);
  UNPROTECT(1);
  return returnvalue;


}

/*****************************************************
 **
 ** SEXP R_bm_getBufferSize(SEXP R_BufferedMatrix)
 **
 ** SEXP R_BufferedMatrix
 **
 ** RETURNS the buffer dimensions as a vector of integers first
 **         element is rows, second element is columns
 **
 *****************************************************/

SEXP R_bm_getBufferSize(SEXP R_BufferedMatrix){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  
  PROTECT(returnvalue=allocVector(INTSXP,2));

  if (Matrix == NULL){ 

    INTEGER(returnvalue)[0] = 0;
    INTEGER(returnvalue)[1] = 0;
    UNPROTECT(1);
    return returnvalue;
  }
  
  INTEGER(returnvalue)[0] = dbm_getBufferRows(Matrix);
  INTEGER(returnvalue)[1] = dbm_getBufferCols(Matrix);
  UNPROTECT(1);
  return returnvalue;


}

/*****************************************************
 **
 ** SEXP R_bm_getValue(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_row, R_col - location in the matrix
 **
 ** RETURNS value stored in BufferedMatrix at specified location
 **         Note that if a location outside the matrix dimensions is 
 **         Specified then NA is returned.
 ** 
 **
 *****************************************************/

SEXP R_bm_getValue(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col){

  
  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
    
  PROTECT(returnvalue=allocVector(REALSXP,1));

  if (Matrix == NULL){ 
    REAL(returnvalue)[0] = R_NaReal;
    UNPROTECT(1); 
    return R_BufferedMatrix;
  }



  if(!dbm_getValue(Matrix,asInteger(R_row), asInteger(R_col), &REAL(returnvalue)[0])){
    REAL(returnvalue)[0] = R_NaReal;
  }

  UNPROTECT(1);
  return returnvalue;

}


/*****************************************************
 **
 ** SEXP R_bm_setValue(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col,SEXP value)
 **
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_row, R_col - location in the matrix
 ** SEXP value - store Numeric value at specified location
 **
 ** RETURNS TRUE if successful
 **         FALSE if unsuccessful
 ** 
 **
 *****************************************************/


SEXP R_bm_setValue(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col, SEXP value){
  
  doubleBufferedMatrix Matrix;
  SEXP returnvalue;
  
  PROTECT(returnvalue=allocVector(LGLSXP,1));


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
    
  if (Matrix == NULL){   
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }



  if(!dbm_setValue(Matrix,asInteger(R_row), asInteger(R_col), REAL(value)[0])){
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }
  
  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;

}
