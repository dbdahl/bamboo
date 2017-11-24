.onLoad <- function(libname, pkgname) {
  .rscalaPackage(pkgname,heap.maximum="512M")
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}

