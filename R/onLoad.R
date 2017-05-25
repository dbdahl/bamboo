.onLoad <- function(libname, pkgname) {
  command.line.options <- c("-J-Xmx1G","-J-XX:ParallelGCThreads=1","-J-XX:ConcGCThreads=1")
  .rscalaPackage(pkgname,command.line.options=command.line.options)
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}

