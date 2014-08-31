#include "doubleBufferedMatrix.h"

#include "math.h"

#include <R.h>



/* library(BufferedMatrix);library.dynam("BufferedMatrix");.C("dbm_c_tester",integer(1)) */


void dbm_c_tester(int *status){

  doubleBufferedMatrix tempBuffMat;
  char prefix[15] = "dbmtest";
  char directory[2] = ".";
  int i,j;
  double temp;

  tempBuffMat= dbm_alloc(1, 1, prefix,directory);
  dbm_setRows(tempBuffMat,5);
  
  for (i = 0; i < 5; i++){
    dbm_AddColumn(tempBuffMat);
  }
  

  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(tempBuffMat));
  Rprintf("Cols: %d\n",dbm_getCols(tempBuffMat));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(tempBuffMat));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(tempBuffMat));

  Rprintf("\n");
  Rprintf("Assigning Values\n");
  for (i =0; i < 5; i++){
    for (j=0; j < 5; j++){
      dbm_setValue(tempBuffMat,i,j,(double)(i+j));
    }
  }

  for (i=0; i < 5; i++){
    for (j=0; j < 5; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");




  Rprintf("Adding Additional Column\n");
  dbm_AddColumn(tempBuffMat);
  

  
  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(tempBuffMat));
  Rprintf("Cols: %d\n",dbm_getCols(tempBuffMat));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(tempBuffMat));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(tempBuffMat));
  



  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
   }
  Rprintf("\n");

  
  Rprintf("Reassigning values\n");
  for (i =4; i >=0; i--){
    for (j=5; j >=0; j--){
      dbm_setValue(tempBuffMat,i,j,j*(5)+i+1);
    }
  }

  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");


  Rprintf("Resizing Buffers\n");

  dbm_ResizeBuffer(tempBuffMat, 3, 3);
  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(tempBuffMat));
  Rprintf("Cols: %d\n",dbm_getCols(tempBuffMat));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(tempBuffMat));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(tempBuffMat));
  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");



  Rprintf("Activating Row Buffer\n");
  dbm_RowMode(tempBuffMat); 
  Rprintf("In row mode: %d\n",dbm_isRowMode(tempBuffMat));
  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");


  Rprintf("Squaring Last Column\n");

  for (i=0; i < 5; i++){
    dbm_getValue(tempBuffMat,i,5,&temp);
    temp = temp*temp;
    dbm_setValue(tempBuffMat,i,5,temp);
  }
    
  
  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");
  

  Rprintf("Square rooting Last Row, then turing off Row Buffer\n");
  
  
  for (j=0; j < 6; j++){
    dbm_getValue(tempBuffMat,4,j,&temp);
    temp = sqrt(temp);
    dbm_setValue(tempBuffMat,4,j,temp);
  }

  dbm_ColMode(tempBuffMat);

  Rprintf("In row mode: %d\n",dbm_isRowMode(tempBuffMat));
  
  dbm_getValue(tempBuffMat,4,0,&temp);
  Rprintf("Checking on value that should be not be in column buffer%f \n",temp);



  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");
  

  Rprintf("Single Indexing. Assign each value its square\n");

  for (i=29; i >=0; i--){
    dbm_setValueSI(tempBuffMat,i,(double)((i+1)*(i+1)));
  }
  
  
  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");
  

  Rprintf("Resizing Buffers Smaller\n");

  dbm_ResizeBuffer(tempBuffMat, 1, 1);
  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(tempBuffMat));
  Rprintf("Cols: %d\n",dbm_getCols(tempBuffMat));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(tempBuffMat));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(tempBuffMat));
  for (i=0; i < 5; i++){
    for (j=0; j < 6; j++){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  Rprintf("Activating Row Mode.\n");
  dbm_RowMode(tempBuffMat); 
  Rprintf("Resizing Buffers\n");
  
  dbm_ResizeBuffer(tempBuffMat, 1, 1);
  Rprintf("Checking dimensions\n");
  Rprintf("Rows: %d\n",dbm_getRows(tempBuffMat));
  Rprintf("Cols: %d\n",dbm_getCols(tempBuffMat));
  Rprintf("Buffer Rows: %d\n",dbm_getBufferRows(tempBuffMat));
  Rprintf("Buffer Cols: %d\n",dbm_getBufferCols(tempBuffMat));
  
  
  Rprintf("Activating ReadOnly Mode.\n");
  
  dbm_setValue(tempBuffMat,0,0,-10.00);
  dbm_setValue(tempBuffMat,0,1,-20.00);
  dbm_setValue(tempBuffMat,1,0,-30.00);

  dbm_ReadOnlyMode(tempBuffMat,1);

  Rprintf("The results of assignment is: %d\n",dbm_setValue(tempBuffMat,0,0,100000.00));

  Rprintf("Printing matrix reversed.\n");
  for (i=4; i >= 0; i--){
    for (j=5; j >= 0; j--){
      dbm_getValue(tempBuffMat,i,j,&temp);
      Rprintf("%f ",temp);
    }
    Rprintf("\n");
  }
  Rprintf("\n");

  dbm_free(tempBuffMat);



}
