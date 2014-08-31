/*****************************************************
 **
 ** file: RBufferedMatrix.c
 **
 ** Copyright (C) 2006    B. M. Bolstad
 **
 ** aim: Register c code routines so that they can be called in other packages.
 **
 ** History
 ** Nov 8, 2006 - Initial version
 **
 *****************************************************/

#include "doubleBufferedMatrix.h"
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>



void R_init_BufferedMatrix(){


  R_RegisterCCallable("BufferedMatrix", "dbm_setRows", (DL_FUNC)dbm_setRows);
  R_RegisterCCallable("BufferedMatrix", "dbm_AddColumn",(DL_FUNC)dbm_AddColumn);
  R_RegisterCCallable("BufferedMatrix", "dbm_ResizeColBuffer", (DL_FUNC)dbm_ResizeColBuffer);
  R_RegisterCCallable("BufferedMatrix", "dbm_ResizeRowBuffer", (DL_FUNC)dbm_ResizeRowBuffer);
  R_RegisterCCallable("BufferedMatrix", "dbm_ResizeBuffer",(DL_FUNC) dbm_ResizeBuffer);
  R_RegisterCCallable("BufferedMatrix", "dbm_RowMode", (DL_FUNC)dbm_RowMode);
  R_RegisterCCallable("BufferedMatrix", "dbm_ColMode", (DL_FUNC)dbm_ColMode);
  R_RegisterCCallable("BufferedMatrix", "dbm_SetPrefix", (DL_FUNC)dbm_SetPrefix);
  R_RegisterCCallable("BufferedMatrix", "dbm_ReadOnlyMode", (DL_FUNC)dbm_ReadOnlyMode);
  R_RegisterCCallable("BufferedMatrix", "dbm_isReadOnlyMode", (DL_FUNC)dbm_isReadOnlyMode);
  R_RegisterCCallable("BufferedMatrix", "dbm_isRowMode", (DL_FUNC)dbm_isRowMode);
  R_RegisterCCallable("BufferedMatrix", "dbm_getValue", (DL_FUNC)dbm_getValue);
  R_RegisterCCallable("BufferedMatrix", "dbm_setValue", (DL_FUNC)dbm_setValue);
  R_RegisterCCallable("BufferedMatrix", "dbm_getValueSI", (DL_FUNC)dbm_getValueSI);
  R_RegisterCCallable("BufferedMatrix", "dbm_setValueSI", (DL_FUNC)dbm_setValueSI);
  R_RegisterCCallable("BufferedMatrix", "dbm_getRows", (DL_FUNC)dbm_getRows);
  R_RegisterCCallable("BufferedMatrix", "dbm_getCols", (DL_FUNC)dbm_getCols);
  R_RegisterCCallable("BufferedMatrix", "dbm_getBufferCols", (DL_FUNC)dbm_getBufferCols);
  R_RegisterCCallable("BufferedMatrix", "dbm_getBufferRows", (DL_FUNC)dbm_getBufferRows);
  R_RegisterCCallable("BufferedMatrix", "dbm_getValueColumn", (DL_FUNC)dbm_getValueColumn);
  R_RegisterCCallable("BufferedMatrix", "dbm_getValueRow", (DL_FUNC)dbm_getValueRow);
  R_RegisterCCallable("BufferedMatrix", "dbm_setValueColumn", (DL_FUNC)dbm_setValueColumn);
  R_RegisterCCallable("BufferedMatrix", "dbm_setValueRow", (DL_FUNC)dbm_setValueRow);
  R_RegisterCCallable("BufferedMatrix", "dbm_getPrefix", (DL_FUNC)dbm_getPrefix);
  R_RegisterCCallable("BufferedMatrix", "dbm_getDirectory", (DL_FUNC)dbm_getDirectory);
  R_RegisterCCallable("BufferedMatrix", "dbm_getFileName", (DL_FUNC)dbm_getFileName);
  R_RegisterCCallable("BufferedMatrix", "dbm_setNewDirectory", (DL_FUNC)dbm_setNewDirectory);
  R_RegisterCCallable("BufferedMatrix", "dbm_copyValues", (DL_FUNC)dbm_copyValues);
  R_RegisterCCallable("BufferedMatrix", "dbm_ewApply", (DL_FUNC)dbm_ewApply);
  R_RegisterCCallable("BufferedMatrix", "dbm_max", (DL_FUNC)dbm_max);
  R_RegisterCCallable("BufferedMatrix", "dbm_min", (DL_FUNC)dbm_min);
  R_RegisterCCallable("BufferedMatrix", "dbm_mean", (DL_FUNC)dbm_mean);
  R_RegisterCCallable("BufferedMatrix", "dbm_sum", (DL_FUNC)dbm_sum);
  R_RegisterCCallable("BufferedMatrix", "dbm_var", (DL_FUNC)dbm_var);
  R_RegisterCCallable("BufferedMatrix", "dbm_rowMeans", (DL_FUNC)dbm_rowMeans);
  R_RegisterCCallable("BufferedMatrix", "dbm_rowSums", (DL_FUNC)dbm_rowSums);
  R_RegisterCCallable("BufferedMatrix", "dbm_rowVars", (DL_FUNC)dbm_rowVars);
  R_RegisterCCallable("BufferedMatrix", "dbm_rowMax", (DL_FUNC)dbm_rowMax);
  R_RegisterCCallable("BufferedMatrix", "dbm_rowMin", (DL_FUNC)dbm_rowMin); 
  R_RegisterCCallable("BufferedMatrix", "dbm_rowMedians", (DL_FUNC)dbm_rowMedians);
  R_RegisterCCallable("BufferedMatrix", "dbm_colMeans", (DL_FUNC)dbm_colMeans);
  R_RegisterCCallable("BufferedMatrix", "dbm_colSums", (DL_FUNC)dbm_colSums);
  R_RegisterCCallable("BufferedMatrix", "dbm_colVars", (DL_FUNC)dbm_colVars);
  R_RegisterCCallable("BufferedMatrix", "dbm_colMax", (DL_FUNC)dbm_colMax);
  R_RegisterCCallable("BufferedMatrix", "dbm_colMin", (DL_FUNC)dbm_colMin);
  R_RegisterCCallable("BufferedMatrix", "dbm_colMedians", (DL_FUNC)dbm_colMedians);
  R_RegisterCCallable("BufferedMatrix", "dbm_colRanges", (DL_FUNC)dbm_colRanges);
  R_RegisterCCallable("BufferedMatrix", "dbm_fileSpaceInUse", (DL_FUNC)dbm_fileSpaceInUse);
  R_RegisterCCallable("BufferedMatrix", "dbm_memoryInUse", (DL_FUNC)dbm_memoryInUse);
}
