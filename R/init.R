.First.lib <- function(libname, pkgname) {

  library.dynam("BufferedMatrix",pkgname,libname,now=FALSE)
  .C("R_init_BufferedMatrix",PACKAGE="BufferedMatrix")

}
