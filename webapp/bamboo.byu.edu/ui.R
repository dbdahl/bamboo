library(shiny)

logEntry("Starting user interface.")

shinyUI(fluidPage(

  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    tags$link(rel = 'shortcut icon', href = 'favicon.ico'),
    tags$script(type="text/javascript", src = "busy.js"),
    tags$script(HTML('
          Shiny.addCustomMessageHandler("jsCode",
            function(message) {
              console.log(message)
              eval(message.code);
            }
          );
        '))
  ),

  verticalLayout(

    titlePanel("Bamboo: Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction"),
    HTML('<img src="bamboo-green.svg" style="float: left; width: 100px;">
          <p>
            Welcome to the Bamboo website which provides software for the methods described in the <a href="https://doi.org/10.1371/journal.pone.0109832">paper</a>:<br><br>
            Q. Li, D. B. Dahl, M. Vannucci, H. Joo, J. W. Tsai (2014), Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction, <b><i>PLOS ONE</i></b>, 9(10), e109832. <a href="https://doi.org/10.1371/journal.pone.0109832">Paper</a><br><br>
            The software is available in two ways: <ol style="list-style-position:inside;"><li>Through the web app below, and</li><li>As the stand-alone R package <a href="https://cran.r-project.org/package=bamboo">bamboo</a>.</li></ol>To install the package, run the following code at the R prompt: <tt>install.packages("bamboo")</tt><br><br>
We ask that you use the R package instead of our web app if you need to obtain secondary structure predictions for many proteins.  Further, the R package can use training data different from those in the paper.<br><br>
            This work is supported by NIH NIGMS R01 GM104972.
          </p>'),
    hr(),
    h3("Inputs:"),
    HTML(paste('<strong>Amino Acid Sequence</strong> (using 1 letter codes without spaces):
    <textarea id="aaseq">',defaults$aaseq,'</textarea>',sep="")),
    br(),
    radioButtons("priorType", strong("Prior Distribution:"),
      c("Noninformative (NonInfo)" = "noninfo",
        "Multiple Sequence Alignment (MSA)" = "msa")),
    conditionalPanel(
      condition = "input.priorType == 'msa'",
      br(),
      HTML('<a href="msa-counts.txt">Example MSA file</a>'),
      fileInput('msaFile', 'Upload a text file (like the example above), where: 1. the first row is a header containing "H", "E", "T", & "C", 2. subsequent rows give the number of times that each of the four secondary structure types is found in that position in the multiple sequence alignment, and 3. elements are separated by white space.', accept=c('text/csv', 'text/plain'))
    ),
    numericInput("nBurnin", strong("Number of samples to discard for burnin:"), defaults$nBurnin),
    numericInput("nSamples", strong("Number of posterior samples to collect:"), defaults$nSamples),
    br(),
    HTML('<strong>Results show below after clicking "Compute".</strong>'),
    actionButton(inputId="compute", label="Compute"),
    div(class="busy",  
      p("Sampling from the posterior distribution..."),
      img(src="gears.svg")
    ),
    h3("Results:"),
    HTML('<strong>Marginal probability (MP) estimate</strong>:'),
    verbatimTextOutput("mpState"),
    HTML('<strong>Maximum <i>a posteriori</i> (MAP) estimate</strong>:'),
    verbatimTextOutput("mapState"),
    HTML('<strong>Marginal probabilities</strong>'),
    plotOutput("plot"),
    br(),
    tableOutput("probabilities"),
    htmlOutput("info"),
    HTML('<p style="text-align:right;">Software by <a href="https://dahl.byu.edu">David B. Dahl</a>.</p>')

  )
))

