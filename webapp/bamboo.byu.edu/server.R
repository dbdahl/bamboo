library(shiny)
library(bamboo)

changeActionButton <- function(id,value,session) {
  session$sendCustomMessage(type="jsCode",
    list(code= paste("$('#",id,"').prop('disabled',",value,")",sep="")))
}

shinyServer(function(input, output, session) {

  logEntry("Starting a new instance.")
  firstTime <- TRUE

  results <- reactive({
    input$compute
    aaseq <- isolate(input$aaseq)
    nSamples <- isolate(input$nSamples)
    nBurnin <- isolate(input$nBurnin)
    if ( aaseq == "" ) stop("Amino acid sequence must be specified.")
    aaseq <- gsub("\\s","",aaseq)
    if ( ! all(grepl("^[ARNDCEQGHILKMFPSTWYV]+$",aaseq)) ) stop("Amino acid sequence must use only A,R,N,D,C,E,Q,G,H,I,L,K,M,F,P,S,T,W,Y,V.")
    prior <- if ( isolate(input$priorType) == "msa" ) {
      inFile <- isolate(input$msaFile)
      if ( is.null(inFile) ) stop("An MSA file must be uploaded when using the MSA prior.")
      data <- read.table(inFile$datapath, header=TRUE)
      if ( ncol(data) != 4 ) stop("The MSA file must have exactly 4 columns.")
      if ( any(names(data) != c("H","E","T","C")) ) stop('The MSA file must have columns "H", "E", "T", "C" in that order.')
      if ( nrow(data) != nchar(gsub("\\s","",aaseq)) ) stop("The MSA file must have the same number of rows as the number of characters in the amino acid sequence.")
      mdata <- as.matrix(data)
      if ( mode(mdata) != "numeric" ) stop("The MSA file does not contain all numeric values.")
      bamboo.priorMSA(mdata)
    } else bamboo.priorNonInfo()
    x <- if ( firstTime ) {
      firstTime <<- FALSE
      defaults$results
    } else {
      if ( nBurnin < 0 ) stop("Burnin must be nonnegative.")
      if ( nSamples < 0 ) stop("Number of samples must be nonnegative.")
      if ( nBurnin >= nSamples ) stop("Burnin must be less than number of samples.")
      if ( nSamples > 10*defaults$nSamples ) stop('Too many samples requested for this web app version.  Please use the R package "bamboo".')
      logEntry(paste("Collecting ",nSamples," samples with burnin of ",nBurnin," for sequence ",aaseq,sep=""))
      changeActionButton("compute","true",session)
      y <- calculate(aaseq,prior,nSamples,nBurnin)
      changeActionButton("compute","false",session)
      logEntry(paste("Finished calculation for ",aaseq,sep=""))
      y
    }
    x
  })

  output$probabilities <- renderTable({
    fm <- results()$fm
    if ( is.null(fm) ) return(NULL)
    cbind(Position=1:nrow(fm$marginalProbabilities),fm$marginalProbabilities)
  },digits=c(0,0,2,2,2,2),include.rownames=FALSE)

  output$mapState <- renderText({
    fm <- results()$fm
    if ( is.null(fm) ) return(NULL)
    fm$mapState
  })

  output$mpState <- renderText({
    fm <- results()$fm
    if ( is.null(fm) ) return(NULL)
    fm$mpState
  })

  output$plot <- renderPlot({
    fm <- results()$fm
    if ( is.null(fm) ) return(NULL)
    ss <- c(
      "MP"=fm$mpState,
      "MAP"=fm$mapState
    )
    plot(fm,ss)
  })

  output$info <- renderUI({
    time <- results()$time
    fm <- results()$fm
    if ( is.null(time) || is.null(fm) ) return(NULL)
    a0 <- paste("Acceptance rate: ",sprintf("%5.4f",fm$countAccepted/(fm$countTotal-fm$countBad)),sep="")
    a1 <- paste("CPU time: ",sprintf("%3.2f",sum(time[1:2]))," seconds",sep="")
    a2 <- paste("Wall time: ",sprintf("%3.2f",sum(time[3]))," seconds",sep="")
    HTML(paste(a0,a1,a2,sep="<br/>"))
  })

})

