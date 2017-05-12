.onLoad <- function(libname, pkgname) {
  .rscalaPackage(pkgname,command.line.options=c("-J-Xmx1G","-J-XX:ParallelGCThreads=1","-J-XX:ConcGCThreads=1"))
}

