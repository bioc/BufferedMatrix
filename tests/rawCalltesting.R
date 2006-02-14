library(BufferedMatrix);library.dynam("BufferedMatrix");

prefix <- "dbmtest"
directory <- getwd()


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_Test_C",P)
.Call("R_bm_Test_C2",P)
.Call("R_bm_Test_C",P)
.Call("R_bm_Test_C2",P)
rm(P)

#P <- .Call("R_bm_Destroy",P)
#.Call("R_bm_Destroy",P)
#.Call("R_bm_Test_C",P)


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_setRows",P,5)
.Call("R_bm_Test_C2",P)
.Call("R_bm_AddColumn",P)
.Call("R_bm_Test_C2",P)
.Call("R_bm_AddColumn",P)
.Call("R_bm_Test_C2",P)
rm(P)



P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_setRows",P,5)
.Call("R_bm_AddColumn",P)
.Call("R_bm_AddColumn",P)
.Call("R_bm_Test_C2",P)

.Call("R_bm_ResizeBuffer",P,5,5)
.Call("R_bm_Test_C2",P)

.Call("R_bm_RowMode",P)
.Call("R_bm_Test_C2",P)

.Call("R_bm_ColMode",P)
.Call("R_bm_Test_C2",P)
rm(P)


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_setRows",P,10)
.Call("R_bm_AddColumn",P)
.Call("R_bm_SetPrefix",P,"BufferedMatrixFile")
.Call("R_bm_AddColumn",P)
.Call("R_bm_AddColumn",P)
dir(pattern="BufferedMatrixFile")
rm(P)
dir(pattern="BufferedMatrixFile")


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_setRows",P,10)
.Call("R_bm_AddColumn",P)
.Call("R_bm_AddColumn",P)
.Call("R_bm_ReadOnlyModeToggle",P)
.Call("R_bm_isReadOnlyMode",P)
.Call("R_bm_ReadOnlyModeToggle",P)
.Call("R_bm_isReadOnlyMode",P)
.Call("R_bm_isRowMode",P)
.Call("R_bm_RowMode",P)
.Call("R_bm_isRowMode",P)
.Call("R_bm_ColMode",P)
.Call("R_bm_isRowMode",P)
rm(P)


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_setRows",P,10)
.Call("R_bm_AddColumn",P)
.Call("R_bm_AddColumn",P)

.Call("R_bm_getSize",P)
.Call("R_bm_getBufferSize",P)
.Call("R_bm_ResizeBuffer",P,5,5)

.Call("R_bm_getBufferSize",P)
.Call("R_bm_ResizeBuffer",P,-1,5)
rm(P)


P <- .Call("R_bm_Create",prefix,directory,1,1)
.Call("R_bm_Test_C",P)
.Call("R_bm_getValue",P,3,3)

.Call("R_bm_getValue",P,100000,10000)
.Call("R_bm_setValue",P,3,3,12345.0)
.Call("R_bm_Test_C2",P)
rm(P)
