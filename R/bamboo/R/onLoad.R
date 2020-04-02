.onLoad <- function(libname, pkgname) {
  assign("s", NULL, envir=parent.env(environment()))
  globalVariables("s")
}

scalaEnsure <- function() {
  if ( ! is.null(s) ) return()
  s <- scala("bamboo")
  scalaLazy(function(s) s + 'import org.ddahl.bamboo._')
  env <- parent.env(environment())
  # unlockBinding("s", env)
  eval(parse(text=paste0('unlockBinding("s",env)')))
  assign("s", s, envir=env)
  lockBinding("s", env)
}

.onUnload <- function(libpath) {
  if ( ! is.null(s) ) close(s)
}
