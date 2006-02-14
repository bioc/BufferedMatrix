#ifndef DOUBLE_BUFFERED_MATRIX_H
#define DOUBLE_BUFFERED_MATRIX_H


/* An opaque pointer to a doubleBufferedMatrix */

typedef struct _double_buffered_matrix *doubleBufferedMatrix;


/* Memory allocation */
doubleBufferedMatrix dbm_alloc(int max_rows, int max_cols, char *prefix, char *directory);
int dbm_free(doubleBufferedMatrix Matrix);


/* Public functions */

int dbm_setRows(doubleBufferedMatrix Matrix, int Rows);
int dbm_AddColumn(doubleBufferedMatrix Matrix);
int dbm_ResizeColBuffer(doubleBufferedMatrix Matrix, int new_maxcol);
int dbm_ResizeRowBuffer(doubleBufferedMatrix Matrix, int new_maxrow);
int dbm_ResizeBuffer(doubleBufferedMatrix Matrix, int new_maxrow, int new_maxcol);
void dbm_RowMode(doubleBufferedMatrix Matrix);
void dbm_ColMode(doubleBufferedMatrix Matrix);

void dbm_SetPrefix(doubleBufferedMatrix Matrix,char *prefix);

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
#endif
