#' @importFrom doParallel registerDoParallel
.onLoad <- function(libname, pkgname){
  registerDoParallel()
}