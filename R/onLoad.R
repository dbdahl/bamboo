.onLoad <- function(libname, pkgname) {
  command.line.options <- c("-J-Xmx1G","-J-XX:ParallelGCThreads=1","-J-XX:ConcGCThreads=1")
  # Note that shQuote below is needed for rscala <= 2.1.1 on Windows.  It can be removed when using later versions of rscala.
  if ( packageVersion("rscala") <= '2.1.1' ) command.line.options <- shQuote(command.line.options)
  .rscalaPackage(pkgname,command.line.options=command.line.options)
}

