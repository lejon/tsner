#' @importFrom rJava .jpackage
.onLoad <- function(libname, pkgname) {
  options(java.parameters='-Xmx4g')
  .jpackage(pkgname, lib.loc = libname)
}
