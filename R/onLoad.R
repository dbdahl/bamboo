.onLoad <- function(libname, pkgname) {
  scalaPackage(pkgname,function(s) s + 'import org.ddahl.bamboo._')
}

.onUnload <- function(libpath) {
  scalaPackageUnload()
}

