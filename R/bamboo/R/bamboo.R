bamboo.priorMSA <- function(countsMatrix,alpha=c(1,1,1,1)) {
  if ( length(alpha) != 4 ) stop("alpha must have length 4.")
  alpha <- c(alpha,0,0,0)
  y <- countsMatrix
  if ( is.null(y) ) {
    if ( var(alpha[1:4]) != 0.0 ) stop("When countsMatrix is not specified, the elements of alpha must be equal.")
    bamboo.priorNonInfo()
  } else {
    if ( ncol(y) == 4 ) y <- cbind(y,0,0,0)
    storage.mode(y) <- "integer"
    ##rJava## .jcall(.jnew("org/ddahl/bamboo/PriorMSA$"),"Lorg/ddahl/bamboo/PriorMSA;","apply",.jarray(y,dispatch=TRUE),as.double(alpha))
    s$PriorMSA(y,alpha)
  }
}

bamboo.priorNonInfo <- function() {
  ##rJava## .jcall(.jnew("org/ddahl/bamboo/PriorNoHE$"),"Lorg/ddahl/bamboo/PriorNoHE;","apply")
  s ^ 'PriorNoHE()'
}

deduce.model <- function(secondary) {
  if ( all(grepl("^[KOS]+$",secondary)) ) "KOS"
  else if ( all(grepl("^[HETC]+$",secondary)) ) "HETC"
  else stop("Secondary must use only characters H,E,T,C or K,O,S.")
}

bamboo.priorMM <- function(secondary,order=(1:9)[7],countsFile=NA,force=FALSE,warn=TRUE) {
  model <- deduce.model(secondary)
  priorStr <- paste("PriorMM",order,sep="")
  ##rJava## priorClass <- .jnew(paste("org/ddahl/bamboo/",priorStr,"$",sep=""))
  priorClass <- priorStr
  if ( is.na(countsFile) ) countsFile <- paste(model,priorStr,sep=.Platform$file.sep)
  ##rJava## bag <- .jcall(.jnew("org/ddahl/bamboo/BagOfPMFs$"),"Lorg/ddahl/bamboo/BagOfPMFs;","apply",FALSE)
  bag <- s$BagOfPMFs(FALSE)
  ##rJava## priorSignature <- paste("Lorg/ddahl/bamboo/",priorStr,";",sep="")
  ##rJava## prior <- .jcall(priorClass,priorSignature,"apply",bag)
  prior <- s(bag=bag) ^ paste0(priorClass,"(bag)")
  if ( force || ! file.exists(countsFile) ) {
    ##rJava## .jcall(prior,"V","count",secondary,countsFile)
    prior$count(secondary,countsFile)
  } else {
    if ( warn ) warning("Using existing counts file, so ignoring secondary argument. Suppress this warning with warn=FALSE.")
  }
  ##rJava## bag <- .jcall(.jnew("org/ddahl/bamboo/BagOfPMFs$"),"Lorg/ddahl/bamboo/BagOfPMFs;","apply",countsFile,FALSE)
  bag <- s$BagOfPMFs(countsFile,FALSE)
  ##rJava## .jcall(priorClass,priorSignature,"apply",bag)
  s(bag=bag) ^ paste0(priorClass,"(bag)")
}

bamboo.likelihood.engine <- function(string,aa=NA) {
  x <- if ( ! is.na(aa) ) {
    if ( ! all(grepl("^[ARNDCEQGHILKMFPSTWYV]+$",aa)) ) stop("Amino acid sequence must use only A,R,N,D,C,E,Q,G,H,I,L,K,M,F,P,S,T,W,Y,V.")
    ##rJava## aaObj <- .jcall(.jnew("org/ddahl/bamboo/AminoAcidSequence$"),"Lorg/ddahl/bamboo/AminoAcidSequence;","apply",aa)
    aaObj <- s$AminoAcidSequence(aa)
    ##rJava## .jcall(.jnew("org/ddahl/bamboo/Likelihood$"),"Lorg/ddahl/bamboo/Likelihood;","apply",string,aaObj)
    s$Likelihood(string,aaObj)
  } else {
    ##rJava## factory <- .jcall("org/ddahl/bamboo/Likelihood","Lscala/Function1;","factory",string)
    factory <- s$Likelihood.factory(string)
    ##rJava## aasd <- .jnew("org/ddahl/bamboo/AminoAcidSequence$")
    function(aa) {
      if ( ! all(grepl("^[ARNDCEQGHILKMFPSTWYV]+$",aa)) ) stop("Amino acid sequence must use only A,R,N,D,C,E,Q,G,H,I,L,K,M,F,P,S,T,W,Y,V.")
      ##rJava## aaObj <- .jcall(aasd,"Lorg/ddahl/bamboo/AminoAcidSequence;","apply",aa)
      aaObj <- s$AminoAcidSequence(aa)
      ##rJava## .jcall(factory,"Lorg/ddahl/bamboo/Likelihood;","apply",aaObj)
      factory(aaObj)
    }
  }
  x
}

bamboo.likelihood <- function(primary,secondary,countsDirectory="HETC",force=FALSE,warn=TRUE) {
  if ( ! all(grepl("^[ARNDCEQGHILKMFPSTWYV]+$",primary)) ) stop("Amino acid sequence must use only A,R,N,D,C,E,Q,G,H,I,L,K,M,F,P,S,T,W,Y,V.")
  model <- deduce.model(secondary)
  if ( is.na(countsDirectory) ) countsDirectory <- model
  if ( model == "KOS" ) {
    str1 <- "K:.;O:.;S:."
    str2 <- gsub("\\*",countsDirectory,"K:*/K;O:*/O;S:*/S","\\*")
  } else if ( model == "HETC" ) {
    str1 <- "H:.:.;E:.;T:.;C:."
    str2 <- gsub("\\*",countsDirectory,"H:*/H:*/H_r;E:*/E;T:*/T;C:*/C","\\*")
  }
  if ( force || ! file.exists(countsDirectory) ) {
    ##rJava## .jcall(bamboo.likelihood.engine(str1,"A"),"V","count",primary,secondary,countsDirectory)
    bamboo.likelihood.engine(str1,"A")$count(primary,secondary,countsDirectory)
  } else {
    if ( warn ) warning("Using existing counts directory, so ignoring primary and secondary arguments. Suppress this warning with warn=FALSE.")
  }
  bamboo.likelihood.engine(str2)
}

bamboo.estimate <- function(likelihood,prior,nSamples,dropFirst,initialState=NULL,doLeastSquaresEstimation=FALSE,dumpStates=FALSE) {
  ##rJava## n <- .jcall(.jcall(likelihood,"Lorg/ddahl/bamboo/AminoAcidSequence;","aa"),"I","nPositions")
  n <- likelihood$aa()$nPositions()
  if ( is.null(initialState) ) {
    keyString <- paste(sort(strsplit(likelihood$blockLikelihoodMap()$keys()$mkString(),"")[[1]]),collapse="")
    if ( keyString == "KOS" ) {
      initialState <- rep("K",n)
      initialState[1:n %% 10 >= 5] <- "O"
      initialState <- paste(initialState,collapse="")
    } else if ( keyString == "CEHT" ) {
      initialState <- rep("C",n)
      initialState[1:n %% 10 >= 5] <- "H"
      initialState <- bamboo.sanitize(paste(initialState,collapse=""))
    } else stop("Unexpected model.")
  } else if ( nchar(initialState) != n ) stop("Length of initial state is not compatible with the likelihood.")
  ##rJava## stateObj <- .jcall("org/ddahl/bamboo/Bamboo","Lorg/ddahl/bamboo/Bamboo;","apply",initialState)
  stateObj <- s$Bamboo(initialState)
  ##rJava## prior <- .jcast(prior,"org/ddahl/bamboo/Prior")
  ##rJava## result <- .jcall("org/ddahl/bamboo/Estimate","Lorg/ddahl/bamboo/MCMCResults;","apply",likelihood,prior,stateObj,as.integer(nSamples),as.integer(dropFirst),as.logical(doLeastSquaresEstimation),as.logical(dumpStates))
  result <- s$Estimate(likelihood,prior,stateObj,as.integer(nSamples),as.integer(dropFirst),as.logical(doLeastSquaresEstimation),as.logical(dumpStates))
  ##rJava## marginalProbabilities = .jevalArray(.jcall(result,"[[D","marginalProbabilities",evalArray=FALSE),simplify=TRUE)
  marginalProbabilities = result$marginalProbabilities()
  ##rJava## colnames(marginalProbabilities) <- .jcall("org/ddahl/bamboo/package","[S","SSOrder")
  colnames(marginalProbabilities) <- s$SSOrder()
  x <- list(
    ##rJava## countTotal      = .jcall(result,"I","countTotal"),
    countTotal      = result$countTotal(),
    ##rJava## countBad        = .jcall(result,"I","countBad"),
    countBad        = result$countBad(),
    ##rJava## countAccepted   = .jcall(result,"I","countAccepted"),
    countAccepted   = result$countAccepted(),
    ##rJava## mapState        = .jcall(.jcall("org/ddahl/bamboo/Bamboo","Lscala/collection/immutable/List;","toSequence",.jcall(result,"Lscala/collection/immutable/List;","mapState")),"S","mkString",""),
    mapState        = s$Bamboo.toSequence(result$mapState())$mkString(""),
    ##rJava## maxLogPosterior = .jcall(result,"D","maxLogPosterior"),
    maxLogPosterior = result$maxLogPosterior(),
    ##rJava## mpState         = .jcall(.jcall("org/ddahl/bamboo/Bamboo","Lscala/collection/immutable/List;","toSequence",.jcall(result,"Lscala/collection/immutable/List;","mpState")), "S","mkString",""),
    mpState         = s$Bamboo.toSequence(result$mpState())$mkString(""),
    ##rJava## lsState         = .jcall(.jcall("org/ddahl/bamboo/Bamboo","Lscala/collection/immutable/List;","toSequence",.jcall(result,"Lscala/collection/immutable/List;","lsState")), "S","mkString",""),
    lsState         = s$Bamboo.toSequence(result$lsState())$mkString(""),
    marginalProbabilities = marginalProbabilities[,c("H","E","T","C")]
  )
  class(x) <- "bamboo.estimate"
  x
}

plot.bamboo.estimate <- function(x,ss=NULL,...) {

  # Setup
  nHeaders <- length(ss)
  headerHeight <- 0.1
  n <- nrow(x$marginalProbabilities)
  if ( length(dev.list()) == 0 ) {
    func <- options()$device
    if ( is.function(func) ) func(width=14,height=6)
  } else {
    ds <- dev.size()
    ar <- ds[1]/ds[2]
    if ( ( ar < 2 ) || ( ar > 3 ) ) warning("For best results, the plotting device should have an aspect ratio about 14:6.  Closing the current device is recommended.")
  }
  par(mar=c(4.2,3.1,0.1,0.1),xpd=NA,...)
  plot(NA,xlim=c(1,1.08*n),ylim=c(0,1+nHeaders*headerHeight),type="n",xlab="Position",ylab=NA,axes=FALSE)

  # Draw axes
  axis(1,at=round(seq(1,n,length=10)))
  x0 <- 1-n/31
  x1 <- 1-n/40
  x2 <- 1-n/50
  mtext("Probability",side=2,line=1.6,at=0.5)
  lines(rep(x2,2),c(0,1))
  for ( i in seq(0,1,length=6) ) {
    lines(c(x1,x2),c(i,i))
    text(x0,i,sprintf("%0.1f",i),adj=1)
  }

  # Show legend
  if ( sum(x$marginalProbabilities[,c("H","E","T","C")]) > 0 ) {
    colors <- list("H"="red","E"="green","T"="blue","C"="black")
    if ( ! all(grepl("^[HETC]+$",ss)) ) stop("Supplied secondary structure doesn't match the marginal probabilities.")
  } else if ( sum(x$marginalProbabilities[,c("K","O","S")]) > 0 ) {
    colors <- list("K"="cyan","O"="magenta","S"="yellow")
    if ( ! all(grepl("^[KOS]+$",ss)) ) stop("Supplied secondary structure doesn't match the marginal probabilities.")
  } else stop("Unexpected marginal probabilities.")
  legend(1.03*n,0.5,legend=names(colors),col=unlist(colors),lty=1,lwd=2,bty="n")

  # Plot lines
  for ( X in names(colors) ) {
    lines(1:n,x$marginalProbabilities[,X],col=colors[[X]],lwd=2.5)
  }

  # Show secondary structure
  ss <- rev(ss)
  for ( header in seq_along(ss) ) {
    for ( i in 1:n ) {
      rect(i-0.5,1+header*headerHeight,i+0.5,1+(header+0.2)*headerHeight,density=NA,border=NA,col=colors[[substring(ss[header],i,i)]])
    }
    text(1.02*n,1+(header+0.1)*headerHeight,names(ss)[header],adj=c(0,0.5))
  }
  return(invisible())

}

