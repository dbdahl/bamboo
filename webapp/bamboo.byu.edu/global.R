dir.create("log",showWarning=FALSE)
logFile <- file.path("log",Sys.getpid())

logEntry <- function(expr,asString=TRUE) {
  capture.output({
    cat(date()," > ",sep="")
    if ( asString ) cat(expr,"\n",sep="")
    else expr
  }, file=logFile, append=TRUE)
}

logEntry("Starting server.")

.libPaths("~/local/R/packages")

library(bamboo)
data(bamboo.training)
likelihood <- bamboo.likelihood(bamboo.training[,"primary"],bamboo.training[,"hetc"],force=FALSE,warn=FALSE)

calculate <- function(aaseq,prior,nSamples,nBurnin) {
  start.time <- proc.time()
  fm <- bamboo.estimate(likelihood(aaseq),prior,nSamples,nBurnin)
  stop.time <- proc.time() - start.time
  list(fm=fm,time=proc.time()-start.time)
}

if ( ! file.exists("defaults.Rbin") ) {
  defaults <- list(
    aaseq="LHSGADDYLTKPFNRNDLLSRIEIHLRTQNYY",
    prior=bamboo.priorNonInfo(),
    #  nSamples=1000,
    #  nBurnin=10
    nSamples=1000000,
    nBurnin=10000
  )
  defaults$results <- calculate(defaults$aaseq,defaults$prior,defaults$nSamples,defaults$nBurnin)
  save(defaults,file="defaults.Rbin")
} else {
  load("defaults.Rbin")
}

logEntry("Server is ready.")

