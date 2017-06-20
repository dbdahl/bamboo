.onLoad <- function(libname, pkgname) {
  .rscalaPackage(pkgname)
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}

