pathOfScript <- function() {
  tryCatch({
      # Works when using source
      dirname(normalizePath(parent.frame(3)$ofile))
    },
    error = function(e) {
      args <- commandArgs()
      # Works when using R CMD BATCH
      t1 <- which(args == '-f')
      if ( length(t1) > 0 ) {
        if ( length(t1) > 1 ) stop("error: too many -f arguments")
        return(dirname(normalizePath(args[t1+1])))
      }
      # Works when using Rscript
      t1 <- grep("^--file=",args)
      if ( length(t1) > 0 ) {
        if ( length(t1) > 1 ) stop("error: too many --file= arguments")
        return(dirname(normalizePath(substring(args[t1],8,nchar(args[t1])))))
      }
      return(getwd())
    }
  )
}

