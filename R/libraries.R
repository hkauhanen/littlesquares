# use this to install packages on condor

install_required <- function(libpath) {
  dir.create(libpath)
  .libPaths(libpath)
  install.packages("ritwals_0.0.0.9000.tar.gz", repos=NULL, lib=libpath)
  install.packages("dyntyp_0.0.0.9000.tar.gz", repos=NULL, lib=libpath)
}

