.onLoad <- function(libname, pkgname) {
  s <- scala(pkgname)
  scalaLazy(function(s) s + 'import org.ddahl.bamboo._')
  assign("s",s,envir=parent.env(environment()))
}

.onUnload <- function(libpath) {
  close(s)
}

