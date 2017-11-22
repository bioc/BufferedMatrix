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
 **  Feb 3, 2006 Add Finalizer
 **  Feb 7, 2006 Add functionality for accessing columns or rows at a time
 **              and returning as an R matrix
 **  Feb 16, 2006 R_bm_getPrefix, R_bm_getDirectory, R_copyValues added
 **  Feb 17, 2006 R_bm_ewApply added, R_bm_ewLog, R_bm_ewPow, R_bm_ewSqrt, R_bm_ewExp added
 **  Feb 22, 2006 R_bm_max, R_bm_min, R_bm_sum, R_bm_var, R_bm_mean, R_bm_ColSums, R_bm_ColMeans, R_bm_RowMeans,  R_bm_RowSums
 **  Apr 26-27, 2006 R_bm_rowApply, R_bm_colApply, R_bm_as_matrix
 **  May 30, 2006 - Add a "tag" to  ExternalPtr with a text name so we can check whether pointer is a BufferedMatrix pointer.
 **                 Added functionality for checking whether a we have a BufferedMatrix or not
 **                 Add R_bm_MakeSubmatrix
 **  Oct 3, 2006 - add R_bm_as_BufferedMatrix   
 **  Oct 21, 2006 - add R_bm_colMedians
 **  Oct 22, 2006 - add R_bm_colRanges
 **  Oct 27, 2006 - add R_bm_getFileNames
 **  Nov 12, 2006 - fix some compiler warnings
 **  Nov 18, 2006 - Increase speed of R_bm_MakeSubmatrix
 **  Sep  9, 2006 - add R_bm_rowMedians
 ** Jan 15, 2009 - fix VECTOR_ELT/STRING_ELT issues
 **
 *****************************************************/

#include "doubleBufferedMatrix.h"

#include <Rdefines.h>
#include <Rinternals.h>

#include <math.h>
#include <string.h>


/* Pre-declare the function which deallocated the c part of the BufferedMatrix */

SEXP R_bm_Destroy(SEXP R_BufferedMatrix);


/*****************************************************
 **
 ** static void R_bm_Finalizer(SEXP R_BufferedMatrix)
 **
 ** This is the Finalizer function that is called 
 ** when the object is deleted on gc() or when R quits.
 ** It deallocates everything and deletes all the 
 ** temporary files etc
 **
 *****************************************************/



static void R_bm_Finalizer(SEXP R_BufferedMatrix){

  R_bm_Destroy(R_BufferedMatrix);

}

/*****************************************************
 **
 ** static int checkBufferedMatrix(SEXP R_BufferedMatrix)
 **
 **
 ** Checks whether the supplied externalpointer is
 ** to a BufferedMatrix
 **
 *****************************************************/

static int checkBufferedMatrix(SEXP R_BufferedMatrix){

  
  SEXP tagsxp;

  char truetagname[15] = "RBufferedMatrix";

  const char *tagname;

 
  tagsxp = R_ExternalPtrTag(R_BufferedMatrix);

  if (!IS_CHARACTER(tagsxp)){
    return 0;
  } else {
    tagname = CHAR(STRING_ELT(tagsxp,0));
    if (strncmp(truetagname,tagname,15) !=0){
      return 0;
    } else {
      return 1;
    }
  }
 
  return 0;
}


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

  const char *prefix = CHAR(STRING_ELT(R_prefix,0));
  const char *directory = CHAR(STRING_ELT(R_directory,0));

  double max_rows = asReal(R_max_rows);
  double max_cols = asReal(R_max_cols);

  SEXP val;
  SEXP tag;


  doubleBufferedMatrix Matrix;

  Matrix = dbm_alloc(max_rows,max_cols,prefix,directory);

  PROTECT(tag = allocVector(STRSXP,1));

  SET_STRING_ELT(tag,0,mkChar("RBufferedMatrix"));

  PROTECT(val = R_MakeExternalPtr(Matrix, tag, R_NilValue));

  R_RegisterCFinalizerEx(val, (R_CFinalizer_t)R_bm_Finalizer,TRUE);


  UNPROTECT(2);
  return val;
}



SEXP R_bm_Test_C(SEXP R_BufferedMatrix){

  int i,j;
  double temp;

  doubleBufferedMatrix Matrix;

  SEXP tempsxp;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }

  tempsxp = R_ExternalPtrTag(R_BufferedMatrix);

  if (IS_CHARACTER(tempsxp)){
    Rprintf("%s\n",CHAR(STRING_ELT(tempsxp,0)));
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

  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_setRows");
  }

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
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_AddColumn");
  }

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

  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_ResizeBuffer");
  }
  
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

  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_RowMode");
  }
 
    
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

  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_ColMode");
  }
 
    
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
  const char *prefix = CHAR(STRING_ELT(R_Prefix,0));

  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_SetPrefix");
  }
 
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
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_ReadOnlyModeToggle");
  }
 

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
  int current_mode=0;
 
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_isReadOnlyMode");
  }


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
 
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_isRowMode");
  }

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
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_getSize");
  }

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
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_getBufferSize");
  }

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







/*****************************************************
 **
 ** SEXP R_bm_getValueColumn(SEXP R_BufferedMatrix, SEXP R_col)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_col - Columns to access in the matrix
 **
 ** RETURNS values stored in BufferedMatrix at specified location
 **         Note that if a location outside the matrix dimensions is 
 **         Specified then NA is returned.
 ** 
 **
 *****************************************************/

SEXP R_bm_getValueColumn(SEXP R_BufferedMatrix, SEXP R_col){


  
  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int i,j;
  int ncols;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  ncols = length(R_col);
    
  PROTECT(returnvalue=allocMatrix(REALSXP,dbm_getRows(Matrix),ncols));

  if (Matrix == NULL){ 
    for (i=0; i < dbm_getRows(Matrix)*ncols; i++){
      REAL(returnvalue)[i] = R_NaReal;
    }
    UNPROTECT(1); 
    return returnvalue;
  }

	  
  if(!dbm_getValueColumn(Matrix, INTEGER(R_col), REAL(returnvalue),ncols)){
    for (j = 0; j < ncols; j++){
      for (i=0; i < dbm_getRows(Matrix); i++){
	REAL(returnvalue)[j*dbm_getRows(Matrix) + i] = R_NaReal;
      }
    }
  }
  
  UNPROTECT(1);
  return returnvalue;


}



/*****************************************************
 **
 ** SEXP R_bm_getValueRow(SEXP R_BufferedMatrix, SEXP R_row)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_row - Rows to access in the matrix
 **
 ** RETURNS values stored in BufferedMatrix at specified location
 **         Note that if a location outside the matrix dimensions is 
 **         Specified then NA is returned.
 ** 
 **
 *****************************************************/

SEXP R_bm_getValueRow(SEXP R_BufferedMatrix, SEXP R_row){
  
  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int i,j;
  int nrows;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  nrows = length(R_row);
  

  PROTECT(returnvalue=allocMatrix(REALSXP,nrows,dbm_getCols(Matrix)));

  if (Matrix == NULL){ 
    for (i=0; i < dbm_getCols(Matrix)*nrows; i++){
      REAL(returnvalue)[i] = R_NaReal;
    }
    UNPROTECT(1); 
    return returnvalue;
  }
  

 
  if(!dbm_getValueRow(Matrix, INTEGER(R_row), REAL(returnvalue), nrows)){
    for (i = 0; i < nrows; i++){
      for (j=0; j < dbm_getCols(Matrix); j++){
	REAL(returnvalue)[j*nrows + i] = R_NaReal;
      }
    } 
  }


  UNPROTECT(1);
  return returnvalue;


}




/*****************************************************
 **
 ** SEXP R_bm_getValueSubmatrix(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_row - Rows to access in the matrix
 ** SEXP R_col - Columns to access in the matrix
 **
 ** RETURNS values stored in BufferedMatrix at specified locations
 **         Note that if a location outside the matrix dimensions is 
 **         Specified then NA is returned.
 ** 
 ** This function gets specified section of the matrix as specified
 ** by rows and columns
 **
 *****************************************************/


SEXP R_bm_getValueSubmatrix(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col){
  

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int i,j;
  int nrows,ncols;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  nrows = length(R_row);
  ncols = length(R_col);
  

  PROTECT(returnvalue=allocMatrix(REALSXP,nrows,ncols));

  if (Matrix == NULL){ 
    for (i=0; i < ncols*nrows; i++){
      REAL(returnvalue)[i] = R_NaReal;
    }
    UNPROTECT(1); 
    return returnvalue;
  }
  
  for (j=0; j < ncols; j++){
    for (i = 0; i < nrows; i++){
      if(!dbm_getValue(Matrix,INTEGER(R_row)[i],  INTEGER(R_col)[j], &REAL(returnvalue)[j*nrows + i])){
	REAL(returnvalue)[j*nrows + i] = R_NaReal;
      }
    }
  }
  UNPROTECT(1); 
  return returnvalue;

}











/*****************************************************
 **
 ** SEXP R_bm_setValueColumn(SEXP R_BufferedMatrix, SEXP R_col, SEXP value)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_col - Columns to access in the matrix
 ** SEXP value - store Numeric values at specified locations
 **
 ** RETURNS TRUE if successful
 **         FALSE if unsuccessful
 **
 ** 
 **
 *****************************************************/

SEXP R_bm_setValueColumn(SEXP R_BufferedMatrix, SEXP R_col, SEXP value){


  
  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  int ncols;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  ncols = length(R_col);
    
  PROTECT(returnvalue=allocVector(LGLSXP,1));

  if (Matrix == NULL){ 
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }

	  
  if(!dbm_setValueColumn(Matrix, INTEGER(R_col), REAL(value), ncols)){ 
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  } 


  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;


}






SEXP R_bm_setValueRow(SEXP R_BufferedMatrix, SEXP R_row, SEXP value){


  
  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  int nrows;

    

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  nrows = length(R_row);
    
  PROTECT(returnvalue=allocVector(LGLSXP,1));

  if (Matrix == NULL){ 
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }

  
  if(!dbm_setValueRow(Matrix, INTEGER(R_row), REAL(value),nrows)){
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }

  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;


}



SEXP R_bm_setValueSubmatrix(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col, SEXP value){



  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int i,j;
  int nrows,ncols;

  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  nrows = length(R_row);
  ncols = length(R_col);
  
  PROTECT(returnvalue=allocVector(LGLSXP,1));
  if (Matrix == NULL){ 
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }


  for (j=0; j < ncols; j++){
    for (i = 0; i < nrows; i++){
      if(!dbm_setValue(Matrix,INTEGER(R_row)[i],  INTEGER(R_col)[j], REAL(value)[j*nrows + i])){
	
	LOGICAL(returnvalue)[0] = FALSE;
	UNPROTECT(1);
	return returnvalue;
      }
    }
  }


  
  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;


}




SEXP R_bm_getPrefix(SEXP R_BufferedMatrix){


  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  char *prefix;

  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }


  prefix = dbm_getPrefix(Matrix);

  PROTECT(returnvalue = allocVector(STRSXP,1));

  SET_STRING_ELT(returnvalue,0,mkChar(prefix));

  


  Free(prefix);
  UNPROTECT(1);
  return returnvalue;

}





SEXP R_bm_getDirectory(SEXP R_BufferedMatrix){


  SEXP returnvalue;
  doubleBufferedMatrix Matrix;

  char *directory;

  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }


  directory = dbm_getDirectory(Matrix);

  PROTECT(returnvalue = allocVector(STRSXP,1));

  SET_STRING_ELT(returnvalue,0,mkChar(directory));

  


  Free(directory);
  UNPROTECT(1);
  return returnvalue;

}







SEXP R_bm_copyValues(SEXP R_BufferedMatrix_target, SEXP R_BufferedMatrix_source){


  SEXP returnvalue;
  doubleBufferedMatrix Matrix_target;
  doubleBufferedMatrix Matrix_source;

  /*  char *directory = NULL; */

  
  Matrix_target =  R_ExternalPtrAddr(R_BufferedMatrix_target);
  Matrix_source =  R_ExternalPtrAddr(R_BufferedMatrix_source);

  /* Check the two supplied BufferedMatrices */
  if (Matrix_target == NULL){
    error("Non valid BufferedMatrix supplied as target\n");
  }
  
  if (Matrix_source == NULL){
    error("Non valid BufferedMatrix supplied as source\n");
  }
  
  if ((dbm_getRows(Matrix_source) != dbm_getRows(Matrix_target)) || (dbm_getCols(Matrix_source) != dbm_getCols(Matrix_target))){
    error("Matrices sizes do not agree. Source dimensions: %d %d Target dimensions: %d %d\n",dbm_getRows(Matrix_source),dbm_getCols(Matrix_source),dbm_getRows(Matrix_target),dbm_getCols(Matrix_target));
  }
  
  
  PROTECT(returnvalue = allocVector(LGLSXP,1));

  
  
  if(!dbm_copyValues(Matrix_target,Matrix_source)){
    LOGICAL(returnvalue)[0] = FALSE;
    UNPROTECT(1);
    return returnvalue;
  }

  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(1);
  return returnvalue;


}




static SEXP Rfn_eval(SEXP x, SEXP f, SEXP rho)
{
  defineVar(install("x"), x, rho);
  return(eval(f, rho));
}






SEXP R_bm_ewApply(SEXP R_BufferedMatrix, SEXP Rfn, SEXP rho){


  SEXP temp;
  SEXP returnvalue;

  doubleBufferedMatrix Matrix;
  int j;


    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the two supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  

  
  PROTECT(temp=allocMatrix(REALSXP,dbm_getRows(Matrix),1));
  PROTECT(returnvalue = allocVector(LGLSXP,1));
  

  for (j=0; j < dbm_getCols(Matrix); j++){
    if(!dbm_getValueColumn(Matrix, &j, REAL(temp),1)){
      LOGICAL(returnvalue)[0] = FALSE;
      UNPROTECT(2);
      return returnvalue;
    }
    temp = Rfn_eval(temp,Rfn,rho);
    if(!dbm_setValueColumn(Matrix, &j, REAL(temp), 1)){ 
      LOGICAL(returnvalue)[0] = FALSE;
      UNPROTECT(2);
      return returnvalue;
    } 
    
  }
  




  LOGICAL(returnvalue)[0] = TRUE;
  UNPROTECT(2);
  return returnvalue;







}




static double bm_log(double x, double *param){  
  return(log(x)/log(param[0]));
}


static double bm_pow(double x, double *param){
  return(pow(x,param[0]));
}


static double bm_sqrt(double x, double *param){
  return(sqrt(x));
}



static double bm_exp(double x, double *param){
  return(exp(x));
}




SEXP R_bm_ewSqrt(SEXP R_BufferedMatrix){
  
  
  doubleBufferedMatrix Matrix;
  double *param=0;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  
  dbm_ewApply(Matrix,&bm_sqrt,param);

  
  
  return R_BufferedMatrix;
}


SEXP R_bm_ewExp(SEXP R_BufferedMatrix){
  
  
  doubleBufferedMatrix Matrix;
  double *param=0;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  
  dbm_ewApply(Matrix,&bm_exp,param);

  
  
  return R_BufferedMatrix;
}









SEXP R_bm_ewPow(SEXP R_BufferedMatrix,SEXP power){
  
  
  doubleBufferedMatrix Matrix;
  double param=0;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  
  param = REAL(power)[0];


  dbm_ewApply(Matrix,&bm_pow,&param);

  
  
  return R_BufferedMatrix;
}


SEXP R_bm_ewLog(SEXP R_BufferedMatrix,SEXP base){
  
  
  doubleBufferedMatrix Matrix;
  double param=0;
    
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  
  param = REAL(base)[0];


  dbm_ewApply(Matrix,&bm_log,&param);

  
  
  return R_BufferedMatrix;
}







SEXP R_bm_max(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;

  int foundfinite;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,1));
  NAflag = LOGICAL(removeNA)[0];

  REAL(returnvalue)[0] = dbm_max(Matrix,NAflag,&foundfinite);

  if (!foundfinite && NAflag){
    warning("No finite arguments to max; returning -Inf");
  }


  UNPROTECT(1);
  return returnvalue;

}





SEXP R_bm_min(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;
  int foundfinite;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,1));
  NAflag = LOGICAL(removeNA)[0];

  REAL(returnvalue)[0] = dbm_min(Matrix,NAflag, &foundfinite);

  if (!foundfinite && NAflag){
    warning("No finite arguments to Min; returning Inf");
  }
    



  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_mean(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,1));
  NAflag = LOGICAL(removeNA)[0];

  REAL(returnvalue)[0] = dbm_mean(Matrix,NAflag);


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_sum(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,1));
  NAflag = LOGICAL(removeNA)[0];

  REAL(returnvalue)[0] = dbm_sum(Matrix,NAflag);


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_var(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,1));
  NAflag = LOGICAL(removeNA)[0];

  REAL(returnvalue)[0] = dbm_var(Matrix,NAflag);


  UNPROTECT(1);
  return returnvalue;

}








SEXP R_bm_rowMeans(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowMeans(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}






SEXP R_bm_rowSums(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowSums(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_rowVars(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowVars(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_rowMax(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowMax(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_rowMin(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowMin(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_rowMedians(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getRows(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_rowMedians(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}


SEXP R_bm_colMeans(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colMeans(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_colSums(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colSums(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_colVars(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colVars(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_colMax(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colMax(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}



SEXP R_bm_colMin(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colMin(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}




SEXP R_bm_colMedians(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocVector(REALSXP,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];

  dbm_colMedians(Matrix,NAflag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}




SEXP R_bm_colRanges(SEXP R_BufferedMatrix,SEXP removeNA){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int NAflag,FiniteFlag;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  PROTECT(returnvalue = allocMatrix(REALSXP,2,dbm_getCols(Matrix)));
  NAflag = LOGICAL(removeNA)[0];
  FiniteFlag = LOGICAL(removeNA)[0];

  dbm_colRanges(Matrix,NAflag,FiniteFlag,REAL(returnvalue));


  UNPROTECT(1);
  return returnvalue;

}










SEXP R_bm_colApply(SEXP R_BufferedMatrix, SEXP return_dim, SEXP Rfn, SEXP rho){


  SEXP temp,temp2;
  SEXP buffsize;
  SEXP returnlist;
  SEXP returnvalue;
  SEXP result;

  doubleBufferedMatrix Matrix;
  int j;


  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the two supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  

  
  PROTECT(temp=allocMatrix(REALSXP,dbm_getRows(Matrix),1));
  PROTECT(returnvalue = allocVector(LGLSXP,1));
  PROTECT(returnlist = allocVector(VECSXP,2));
  
  SET_VECTOR_ELT(returnlist,0,returnvalue);



  if (INTEGER(return_dim)[0] == 1){
    PROTECT(result = allocVector(VECSXP,dbm_getCols(Matrix)));
  } else {
    PROTECT(buffsize=allocVector(INTSXP,1));
    INTEGER(buffsize)[0] = 1; //dbm_getCols(Matrix);
    
    PROTECT(result = R_bm_Create(R_bm_getPrefix(R_BufferedMatrix),
				 R_bm_getDirectory(R_BufferedMatrix),
				 buffsize,
				 buffsize
				 ));

    R_bm_setRows(result,return_dim);

    for (j=0; j < dbm_getCols(Matrix); j++){
      R_bm_AddColumn(result);
    }


  }
  
  SET_VECTOR_ELT(returnlist,1,result);

  for (j=0; j < dbm_getCols(Matrix); j++){
    if(!dbm_getValueColumn(Matrix, &j, REAL(temp),1)){
      LOGICAL(returnvalue)[0] = FALSE;
      UNPROTECT(5);
      return returnvalue;
    }
    if (INTEGER(return_dim)[0] ==1){
      SET_VECTOR_ELT(result,j,Rfn_eval(temp,Rfn,rho));
    } else {
      
      PROTECT(temp2 = Rfn_eval(temp,Rfn,rho));

      dbm_setValueColumn(R_ExternalPtrAddr(result), &j, REAL(temp2),1);
      UNPROTECT(1);

    }
  }
  



  LOGICAL(returnvalue)[0] = TRUE;

  if (INTEGER(return_dim)[0] ==1){
    UNPROTECT(4);
  } else {
    UNPROTECT(5);
  }
  return returnlist;







}











SEXP R_bm_rowApply(SEXP R_BufferedMatrix, SEXP return_dim, SEXP Rfn, SEXP rho){


  SEXP temp,temp2;
  SEXP buffsize;
  SEXP returnlist;
  SEXP returnvalue;
  SEXP result;

  doubleBufferedMatrix Matrix;
  int i;


  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  /* Check the two supplied BufferedMatrices */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  

  
  PROTECT(temp=allocMatrix(REALSXP,dbm_getCols(Matrix),1));
  PROTECT(returnvalue = allocVector(LGLSXP,1));
  PROTECT(returnlist = allocVector(VECSXP,2));
  
  SET_VECTOR_ELT(returnlist,0,returnvalue);



  if (INTEGER(return_dim)[0] == 1){
    PROTECT(result = allocVector(VECSXP,dbm_getRows(Matrix)));
  } else {
    PROTECT(buffsize=allocVector(INTSXP,1));
    INTEGER(buffsize)[0] = 1; //dbm_getCols(Matrix);
    
    PROTECT(result = R_bm_Create(R_bm_getPrefix(R_BufferedMatrix),
				 R_bm_getDirectory(R_BufferedMatrix),
				 buffsize,
				 buffsize
				 ));

    R_bm_setRows(result,return_dim);

    for (i=0; i < dbm_getRows(Matrix); i++){
      R_bm_AddColumn(result);
    }


  }
  
  SET_VECTOR_ELT(returnlist,1,result);

  for (i=0; i < dbm_getRows(Matrix); i++){
    if(!dbm_getValueRow(Matrix, &i, REAL(temp),1)){
      LOGICAL(returnvalue)[0] = FALSE;
      UNPROTECT(5);
      return returnvalue;
    }
    if (INTEGER(return_dim)[0] ==1){
      SET_VECTOR_ELT(result,i,Rfn_eval(temp,Rfn,rho));
    } else {
      
      PROTECT(temp2 = Rfn_eval(temp,Rfn,rho));

      dbm_setValueColumn(R_ExternalPtrAddr(result), &i, REAL(temp2),1);
      UNPROTECT(1);

    }
  }
  



  LOGICAL(returnvalue)[0] = TRUE;

  if (INTEGER(return_dim)[0] ==1){
    UNPROTECT(4);
  } else {
    UNPROTECT(5);
  }
  return returnlist;







}





SEXP R_bm_as_matrix(SEXP R_BufferedMatrix){

  doubleBufferedMatrix Matrix;

  int rows, cols;
  int j;

  SEXP RMatrix;

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);


  /* Check the supplied BufferedMatrices actually allocated */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  rows = dbm_getRows(Matrix);
  cols = dbm_getCols(Matrix);


  PROTECT(RMatrix = allocMatrix(REALSXP,rows,cols));

  for (j=0; j < cols; j++){
    dbm_getValueColumn(Matrix, &j, &REAL(RMatrix)[j*rows],1);
  }




  UNPROTECT(1);
  return RMatrix;

}



SEXP isBufferedMatrix(SEXP R_BufferedMatrix){

  
  SEXP tagsxp;
  SEXP returnvalue;

  char truetagname[15] = "RBufferedMatrix";
  const char *tagname;

 


  tagsxp = R_ExternalPtrTag(R_BufferedMatrix);


  PROTECT(returnvalue = allocVector(LGLSXP,1));

  if (!IS_CHARACTER(tagsxp)){
    LOGICAL(returnvalue)[0] = FALSE;
  } else {
    tagname = CHAR(STRING_ELT(tagsxp,0));
    if (strncmp(truetagname,tagname,15) !=0){
      LOGICAL(returnvalue)[0] = FALSE;
    } else {
      LOGICAL(returnvalue)[0] = TRUE;
    }
  }
  


  UNPROTECT(1);
  return returnvalue;
}






/*****************************************************
 **
 ** SEXP R_bm_MakeSubmatrix(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col)
 **
 ** SEXP R_BufferedMatrix
 ** SEXP R_row - Rows to access in the matrix
 ** SEXP R_col - Columns to access in the matrix
 **
 ** RETURNS values stored in BufferedMatrix at specified locations
 **         Note that if a location outside the matrix dimensions is 
 **         Specified then NA is returned.
 ** 
 ** This function gets specified section of the matrix as specified
 ** by rows and columns and returns it stored in another BufferedMatrix
 **
 *****************************************************/


SEXP R_bm_MakeSubmatrix(SEXP R_BufferedMatrix, SEXP R_row, SEXP R_col){
  

  SEXP returnvalue;

  SEXP temp;
  SEXP temp2;


  doubleBufferedMatrix Matrix;
  doubleBufferedMatrix destMatrix;

  int i,j;
  int nrows,ncols;

  int was_writeable =0;

  double tempbuffer;


  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);

  nrows = length(R_row);
  ncols = length(R_col);
  
  PROTECT(temp2 = allocVector(INTSXP,1));
  INTEGER(temp2)[0] = 1;
  

  PROTECT(returnvalue=R_bm_Create(R_bm_getPrefix(R_BufferedMatrix),
				  R_bm_getDirectory(R_BufferedMatrix),
				  temp2,temp2));

  
  PROTECT(temp = allocVector(INTSXP,1));
  INTEGER(temp)[0] = nrows;
  R_bm_setRows(returnvalue,temp);
  UNPROTECT(1);
  
  destMatrix = R_ExternalPtrAddr(returnvalue);


  if (Matrix != NULL){
    if (!dbm_isReadOnlyMode(Matrix)){
      was_writeable = 1;
      dbm_ReadOnlyMode(Matrix,1);
    }
  }

  for (j = 0; j < ncols; j++){
    dbm_AddColumn(destMatrix);  
    if (Matrix == NULL){ 
      for (i = 0; i < nrows; i++){
	tempbuffer = R_NaReal;
	dbm_setValue(destMatrix,i,j,tempbuffer);
      }
    } else {
      for (i = 0; i < nrows; i++){
	if(!dbm_getValue(Matrix,INTEGER(R_row)[i],  INTEGER(R_col)[j], &tempbuffer)){
	  tempbuffer = R_NaReal;
	}
	dbm_setValue(destMatrix,i,j,tempbuffer);
      }
    }
  }

  if (Matrix != NULL){
    if(was_writeable)
      dbm_ReadOnlyMode(Matrix,0);
  }


  UNPROTECT(2); 
  return returnvalue;

}









SEXP R_bm_as_BufferedMatrix(SEXP R_BufferedMatrix, SEXP RMatrix){

  doubleBufferedMatrix Matrix;
  
  int rows, cols;
  int j;

  /*  int rows_RMatrix;
      int cols_RMatrix; */
  

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);


  /* Check the supplied BufferedMatrices actually allocated */
  if (Matrix == NULL){
    error("Non valid BufferedMatrix supplied.\n");
  }
  
  rows = dbm_getRows(Matrix);
  cols = dbm_getCols(Matrix);


  for (j=0; j < cols; j++){
    dbm_setValueColumn(Matrix, &j, &REAL(RMatrix)[j*rows],1);
  }
  

  
  return R_BufferedMatrix;

}









SEXP R_bm_getFileNames(SEXP R_BufferedMatrix){


  SEXP returnvalue;
  doubleBufferedMatrix Matrix;
  int i;


  char *filename;

  
  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  if (Matrix == NULL){
    return R_BufferedMatrix;
  }


  PROTECT(returnvalue = allocVector(STRSXP,dbm_getCols(Matrix)));


  for (i =0; i < dbm_getCols(Matrix); i++){

    filename = dbm_getFileName(Matrix,i);
    SET_STRING_ELT(returnvalue,i,mkChar(filename));
    Free(filename);
  }
  UNPROTECT(1);
  return returnvalue;

}





SEXP R_bm_memoryInUse(SEXP R_BufferedMatrix){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix; 
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_memoryInUse");
  }

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  
  PROTECT(returnvalue=allocVector(INTSXP,1));

  if (Matrix == NULL){ 

    INTEGER(returnvalue)[0] = 0;
    UNPROTECT(1);
    return returnvalue;
  }
  
  INTEGER(returnvalue)[0] = dbm_memoryInUse(Matrix);
  UNPROTECT(1);
  return returnvalue;


}






SEXP R_bm_fileSpaceInUse(SEXP R_BufferedMatrix){

  SEXP returnvalue;
  doubleBufferedMatrix Matrix; 
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_fileSpaceInUse");
  }

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  
  PROTECT(returnvalue=allocVector(REALSXP,1));

  if (Matrix == NULL){ 

    NUMERIC_POINTER(returnvalue)[0] = 0.0;
    UNPROTECT(1);
    return returnvalue;
  }
  
  NUMERIC_POINTER(returnvalue)[0] = dbm_fileSpaceInUse(Matrix);
  UNPROTECT(1);
  return returnvalue;


}





SEXP R_bm_setNewDirectory(SEXP R_BufferedMatrix, SEXP R_new_directory){

  SEXP returnvalue= R_BufferedMatrix;
  doubleBufferedMatrix Matrix; 
  const char *newdirectory = CHAR(STRING_ELT(R_new_directory,0));
  
  
  if(!checkBufferedMatrix(R_BufferedMatrix)){
    error("Invalid ExternalPointer supplied to R_bm_setNewDirectory");
  }

  Matrix =  R_ExternalPtrAddr(R_BufferedMatrix);
  
  dbm_setNewDirectory(Matrix, newdirectory);




  return returnvalue;


}

