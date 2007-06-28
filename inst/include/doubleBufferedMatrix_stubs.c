#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "doubleBufferedMatrix.h"



int dbm_setRows(doubleBufferedMatrix Matrix, int Rows){

  static int(*fun)(doubleBufferedMatrix,int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix,int))R_GetCCallable("BufferedMatrix","dbm_setRows");
  
  return fun(Matrix,Rows);
}


int dbm_AddColumn(doubleBufferedMatrix Matrix){
 static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_AddColumn");
  
  return fun(Matrix);
}



int dbm_ResizeColBuffer(doubleBufferedMatrix Matrix, int new_maxcol){


  static int(*fun)(doubleBufferedMatrix, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_ResizeColBuffer");
  
  return fun(Matrix,new_maxcol);

}




int dbm_ResizeRowBuffer(doubleBufferedMatrix Matrix, int new_maxrow){
  static int(*fun)(doubleBufferedMatrix, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_ResizeRowBuffer");
  
  return fun(Matrix,new_maxrow);

}



int dbm_ResizeBuffer(doubleBufferedMatrix Matrix, int new_maxrow, int new_maxcol){


  static int(*fun)(doubleBufferedMatrix, int,int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int,int))R_GetCCallable("BufferedMatrix","dbm_ResizeBuffer");
  
  return fun(Matrix,new_maxrow,new_maxcol);
  
}



void dbm_RowMode(doubleBufferedMatrix Matrix){

  static void(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_RowMode");

  fun(Matrix);  
  return;
}



void dbm_ColMode(doubleBufferedMatrix Matrix){

 static void(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_ColMode");

  fun(Matrix);  
  return;


}








void dbm_SetPrefix(doubleBufferedMatrix Matrix,char *prefix){



  static void(*fun)(doubleBufferedMatrix, char *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, char *))R_GetCCallable("BufferedMatrix","dbm_SetPrefix");

  fun(Matrix, prefix);  
  return;


}



void dbm_ReadOnlyMode(doubleBufferedMatrix Matrix, int setting){


  static void(*fun)(doubleBufferedMatrix, int) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_ReadOnlyMode");

  fun(Matrix,setting);  
  return;
}

int dbm_isReadOnlyMode(doubleBufferedMatrix Matrix){

  static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_isReadOnlyMode");
  
  return fun(Matrix);

}






int dbm_isRowMode(doubleBufferedMatrix Matrix){

 static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_isRowMode");
  
  return fun(Matrix);

}

int dbm_getValue(doubleBufferedMatrix Matrix, int row, int col, double *value){


  static int(*fun)(doubleBufferedMatrix, int, int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int, int, double *))R_GetCCallable("BufferedMatrix","dbm_getValue");
  
  return fun(Matrix,row,col,value);


}



int dbm_setValue(doubleBufferedMatrix Matrix, int row, int col, double value){

  static int(*fun)(doubleBufferedMatrix, int, int, double) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int, int, double))R_GetCCallable("BufferedMatrix","dbm_setValue");
  
  return fun(Matrix,row,col,value);
}

int dbm_getValueSI(doubleBufferedMatrix Matrix, int index, double *value){

  static int(*fun)(doubleBufferedMatrix, int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int, double * ))R_GetCCallable("BufferedMatrix","dbm_getValueSI");
  
  return fun(Matrix,index,value);
}


int dbm_setValueSI(doubleBufferedMatrix Matrix, int index, double value){



  static int(*fun)(doubleBufferedMatrix, int, double) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int, double))R_GetCCallable("BufferedMatrix","dbm_setValueSI");
  
  return fun(Matrix,index,value);
}



int dbm_getRows(doubleBufferedMatrix Matrix){

  static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getRows");
  
  return fun(Matrix);
}


int dbm_getCols(doubleBufferedMatrix Matrix){

 static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getCols");
  
  return fun(Matrix);
}









int dbm_getBufferCols(doubleBufferedMatrix Matrix){


 static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getBufferCols");
  
  return fun(Matrix);

}





int dbm_getBufferRows(doubleBufferedMatrix Matrix){

  static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getBufferRows");
  
  return fun(Matrix);
}



int dbm_getValueColumn(doubleBufferedMatrix Matrix, int *cols, double *value, int ncol){

  static int(*fun)(doubleBufferedMatrix, int *, double*, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int *, double*, int))R_GetCCallable("BufferedMatrix","dbm_getValueColumn");
  
  return fun(Matrix,cols,value,ncol);
}


int dbm_getValueRow(doubleBufferedMatrix Matrix, int *rows, double *value, int nrows){

  static int(*fun)(doubleBufferedMatrix, int *, double*, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int *, double*, int))R_GetCCallable("BufferedMatrix","dbm_getValueRow");
  
  return fun(Matrix,rows,value,nrows);
}


int dbm_setValueColumn(doubleBufferedMatrix Matrix, int *cols, double *value, int ncols){

 static int(*fun)(doubleBufferedMatrix, int *, double*, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int *, double*, int))R_GetCCallable("BufferedMatrix","dbm_setValueColumn");
  
  return fun(Matrix,cols,value,ncols);
}



int dbm_setValueRow(doubleBufferedMatrix Matrix, int *rows, double *value, int nrows){

  static int(*fun)(doubleBufferedMatrix, int *, double*, int) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, int *, double*, int))R_GetCCallable("BufferedMatrix","dbm_setValueRow");
  
  return fun(Matrix,rows,value,nrows);
}






char *dbm_getPrefix(doubleBufferedMatrix Matrix){


  static char *(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (char *(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getPrefix");
  
  return fun(Matrix);

}



char *dbm_getDirectory(doubleBufferedMatrix Matrix){


  static char *(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (char *(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_getDirectory");
  
  return fun(Matrix);

}



char *dbm_getFileName(doubleBufferedMatrix Matrix, int col){

  static char *(*fun)(doubleBufferedMatrix,int) = NULL;
  
  if (fun == NULL)
    fun =  (char *(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_getFileName");
  
  return fun(Matrix,col);

}


int dbm_copyValues(doubleBufferedMatrix Matrix_target,doubleBufferedMatrix Matrix_source){

 static int(*fun)(doubleBufferedMatrix,doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix, doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_copyValues");
  
  return fun(Matrix_target,Matrix_source);

}


int dbm_ewApply(doubleBufferedMatrix Matrix,double (* fn)(double, double *),double *fn_param){


  static int(*fun)(doubleBufferedMatrix,double (*)(double, double *),double *) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix,double (*)(double, double *),double *))R_GetCCallable("BufferedMatrix","dbm_ewApply");
  
  return fun(Matrix,fn, fn_param);

}


double dbm_max(doubleBufferedMatrix Matrix,int naflag,int *foundfinite){

  static double(*fun)(doubleBufferedMatrix,int, int *) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix, int, int *))R_GetCCallable("BufferedMatrix","dbm_max");
  
  return fun(Matrix,naflag,foundfinite);
}




double dbm_min(doubleBufferedMatrix Matrix,int naflag,int *foundfinite){

 static double(*fun)(doubleBufferedMatrix,int, int *) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix, int, int *))R_GetCCallable("BufferedMatrix","dbm_min");
  
  return fun(Matrix,naflag,foundfinite);

}


double dbm_mean(doubleBufferedMatrix Matrix,int naflag){

 static double(*fun)(doubleBufferedMatrix,int) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_mean");
  
  return fun(Matrix,naflag);
}


double dbm_sum(doubleBufferedMatrix Matrix,int naflag){

  static double(*fun)(doubleBufferedMatrix,int) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_sum");
  
  return fun(Matrix,naflag);




}



double dbm_var(doubleBufferedMatrix Matrix,int naflag){

  static double(*fun)(doubleBufferedMatrix,int) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix, int))R_GetCCallable("BufferedMatrix","dbm_var");
  
  return fun(Matrix,naflag);

}



void dbm_rowMeans(doubleBufferedMatrix Matrix,int naflag,double *results){

  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_rowMeans");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_rowSums(doubleBufferedMatrix Matrix,int naflag,double *results){
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_rowSums");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_rowVars(doubleBufferedMatrix Matrix,int naflag,double *results){
  
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_rowVars");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_rowMax(doubleBufferedMatrix Matrix,int naflag,double *results){

  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_rowMax");

  fun(Matrix,naflag,results);  
  return;





}


void dbm_rowMin(doubleBufferedMatrix Matrix,int naflag,double *results){
  
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_rowMin");

  fun(Matrix,naflag,results);  
  return;

}

void dbm_colMeans(doubleBufferedMatrix Matrix,int naflag,double *results){

  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colMeans");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_colSums(doubleBufferedMatrix Matrix,int naflag,double *results){

  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colSums");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_colVars(doubleBufferedMatrix Matrix,int naflag,double *results){
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colVars");
 
  fun(Matrix,naflag,results); 
  return;
}


void dbm_colMax(doubleBufferedMatrix Matrix,int naflag,double *results){
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colMax");
 
  fun(Matrix,naflag,results); 
  return;
}

void dbm_colMin(doubleBufferedMatrix Matrix,int naflag,double *results){
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colMin");
 
  fun(Matrix,naflag,results); 
  return;
}


void dbm_colMedians(doubleBufferedMatrix Matrix,int naflag,double *results){
  static void(*fun)(doubleBufferedMatrix,int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, double *))R_GetCCallable("BufferedMatrix","dbm_colMedians");

  fun(Matrix,naflag,results);  
  return;
}


void dbm_colRanges(doubleBufferedMatrix Matrix,int naflag, int finite, double *results){
  static void(*fun)(doubleBufferedMatrix,int, int, double *) = NULL;
  
  if (fun == NULL)
    fun =  (void(*)(doubleBufferedMatrix, int, int, double *))R_GetCCallable("BufferedMatrix","dbm_colRanges");

  fun(Matrix,naflag, finite ,results);  
  return;
}




double dbm_fileSpaceInUse(doubleBufferedMatrix Matrix){
 static double(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (double(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_fileSpaceInUse");
  
  return fun(Matrix);
}


int dbm_memoryInUse(doubleBufferedMatrix Matrix){
 static int(*fun)(doubleBufferedMatrix) = NULL;
  
  if (fun == NULL)
    fun =  (int(*)(doubleBufferedMatrix))R_GetCCallable("BufferedMatrix","dbm_memoryInUse");
  
  return fun(Matrix);






}
