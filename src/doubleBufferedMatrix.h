#ifndef DOUBLE_BUFFERED_MATRIX_H
#define DOUBLE_BUFFERED_MATRIX_H


/* An opaque pointer to a doubleBufferedMatrix */

typedef struct _double_buffered_matrix *doubleBufferedMatrix;


/* Memory allocation */
doubleBufferedMatrix dbm_alloc(int max_rows, int max_cols, const char *prefix, const char *directory);
int dbm_free(doubleBufferedMatrix Matrix);


/* Public functions */

int dbm_setRows(doubleBufferedMatrix Matrix, int Rows);
int dbm_AddColumn(doubleBufferedMatrix Matrix);
int dbm_ResizeColBuffer(doubleBufferedMatrix Matrix, int new_maxcol);
int dbm_ResizeRowBuffer(doubleBufferedMatrix Matrix, int new_maxrow);
int dbm_ResizeBuffer(doubleBufferedMatrix Matrix, int new_maxrow, int new_maxcol);
void dbm_RowMode(doubleBufferedMatrix Matrix);
void dbm_ColMode(doubleBufferedMatrix Matrix);

void dbm_SetPrefix(doubleBufferedMatrix Matrix,const char *prefix);

void dbm_ReadOnlyMode(doubleBufferedMatrix Matrix, int setting);

int dbm_isReadOnlyMode(doubleBufferedMatrix Matrix);
int dbm_isRowMode(doubleBufferedMatrix Matrix);

int dbm_getValue(doubleBufferedMatrix Matrix, int row, int col, double *value);
int dbm_setValue(doubleBufferedMatrix Matrix, int row, int col, double value);

int dbm_getValueSI(doubleBufferedMatrix Matrix, int index, double *value);
int dbm_setValueSI(doubleBufferedMatrix Matrix, int index, double value);

int dbm_getRows(doubleBufferedMatrix Matrix);  /* returns how many rows are currently in matrix */
int dbm_getCols(doubleBufferedMatrix Matrix);  /* returns how many cols are currently in matrix */

int dbm_getBufferCols(doubleBufferedMatrix Matrix);  /* returns how many columns are currently in the column buffer */
int dbm_getBufferRows(doubleBufferedMatrix Matrix);  /* returns how many rows are currently in the row buffer */

int dbm_getValueColumn(doubleBufferedMatrix Matrix, int *cols, double *value, int ncol);
int dbm_getValueRow(doubleBufferedMatrix Matrix, int *rows, double *value, int nrows);
int dbm_setValueColumn(doubleBufferedMatrix Matrix, int *cols, double *value, int ncols);
int dbm_setValueRow(doubleBufferedMatrix Matrix, int *rows, double *value, int nrows);



char *dbm_getPrefix(doubleBufferedMatrix Matrix);
char *dbm_getDirectory(doubleBufferedMatrix Matrix);
char *dbm_getFileName(doubleBufferedMatrix Matrix, int col);

int dbm_setDirectory(doubleBufferedMatrix Matrix, char *newdirectory);


int dbm_copyValues(doubleBufferedMatrix Matrix_target,doubleBufferedMatrix Matrix_source);
int dbm_ewApply(doubleBufferedMatrix Matrix,double (* fn)(double, double *),double *fn_param);

double dbm_max(doubleBufferedMatrix Matrix,int naflag,int *foundfinite);
double dbm_min(doubleBufferedMatrix Matrix,int naflag,int *foundfinite);
double dbm_mean(doubleBufferedMatrix Matrix,int naflag);
double dbm_sum(doubleBufferedMatrix Matrix,int naflag);
double dbm_var(doubleBufferedMatrix Matrix,int naflag);


void dbm_rowMeans(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_rowSums(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_rowVars(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_rowMax(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_rowMin(doubleBufferedMatrix Matrix,int naflag,double *results);


void dbm_colMeans(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colSums(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colVars(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colMax(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colMin(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colMedians(doubleBufferedMatrix Matrix,int naflag,double *results);
void dbm_colRanges(doubleBufferedMatrix Matrix,int naflag, int finite, double *results);

double dbm_fileSpaceInUse(doubleBufferedMatrix Matrix);
int dbm_memoryInUse(doubleBufferedMatrix Matrix);

int dbm_setNewDirectory(doubleBufferedMatrix Matrix, const char *newdirectory);


#endif
