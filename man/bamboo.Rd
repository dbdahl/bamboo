\name{bamboo.estimate}
\alias{bamboo.estimate}
\alias{bamboo.likelihood}
%\alias{bamboo.priorMM}
\alias{bamboo.priorNonInfo}
\alias{bamboo.priorMSA}
\alias{plot.bamboo.estimate}
\alias{bamboo}
\title{Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction}
\description{
These functions implement the methodology described in the paper "Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction" cited below.  The main function is \code{bamboo.estimate}, whose arguments are results from \code{bamboo.priorMSA}, \code{bamboo.priorNoHE}, and \code{bamboo.likelihood} functions.  A \code{plot} method is provided to produce figures like those in the paper using results of the  \code{bamboo.estimate} function.  The \code{bamboo.prepare} function ensures that the necessary dependencies are loaded (and is automatically called by the other functions).
}
\usage{
bamboo.likelihood(primary,secondary,countsDirectory="HETC",force=FALSE,warn=TRUE)
bamboo.priorMSA(countsMatrix,alpha=c(1,1,1,1))
%bamboo.priorMM(secondary,countsFile=NA,force=FALSE,warn=TRUE)
bamboo.priorNonInfo()
bamboo.estimate(likelihood,prior,nSamples,dropFirst,initialState=NULL,
                doLeastSquaresEstimation=FALSE,dumpStates=FALSE)
\method{plot}{bamboo.estimate}(x,ss=NULL,...)
}
\arguments{
  \item{primary}{A character vector (whose length is that same as \code{\var{secondary}}) that gives the amino acid sequences (using 1-letter amino acid codes) used to train the sampling model.}
  \item{secondary}{A character vector (whose length is that same as \code{\var{primary}}) that gives the secondary structure sequences (using 1-letter codes) used to train the sampling model and the MM prior.}
  \item{countsDirectory}{A name of the directory to use for storing count files.
%If \code{NA}, either \code{"HETC"} or \code{"KOS"} is used, depending on the \code{\var{secondary}} argument.
}
  \item{countsMatrix}{An L-by-4 matrices, where L is the protein length.  Row l (where l=1,...,L) gives the number of times that the secondary structure of the aligned proteins is H, E, T, and C, respectively.}
  \item{alpha}{A numeric vector of four strictly-positive real values for the Dirichlet prior in the MSA prior.}
%  \item{countsFile}{A name of the file to use for storing the \code{"priorMM"} count file.  If \code{NA}, either \code{"HETC/priorMM"} or \code{"KOS/priorMM"} is used, depending on the \code{\var{secondary}} argument.}
  \item{force}{A logical indicating that, in the case that the count files already exists, the counting should be performed again away.  This is necessary if the \code{\var{primary}} or \code{\var{secondary}} arguments have changed since the last counting.}
  \item{warn}{A logical indicating that, in the case that the count files already exists, a warning should be displayed indicating that the counting is not performed again. Recounting is necessary if the \code{\var{primary}} or \code{\var{secondary}} arguments have changed since the last counting.}
  \item{likelihood}{The result obtained by evaluating the function returned by \code{bamboo.likelihood} for an amino acid sequence encoded as a character vector of length 1 using 1-letter amino acid codes.}
  \item{prior}{The result of a call to the \code{bamboo.priorMSA} function or the \code{bamboo.priorNonInfo} function.}
  \item{nSamples}{An integer giving the number of MCMC samples after burnin to use for inference.}
  \item{dropFirst}{An integer giving the number of MCMC samples to discard as burnin.}
  \item{initialState}{A character vector of length 1 giving the secondary structure state to start the Markov chain Monte Carlo algorithm.  If \code{NULL}, a reasonable default is used.}
  \item{doLeastSquaresEstimation}{A logical implementing an undocumented estimation method.}
  \item{dumpStates}{A logical implementing whether secondary structure states should be printed to standard output (stdout).  This feature is not intended for normal usage and the output is only likely to be seen when running R on a Linux terminal.}
  \item{x}{The result from a call to the \code{bamboo.estimate} function.}
  \item{ss}{A character vector of arbitrary length giving secondary structures to display above the marginal probabilities.  The names of the elements of the vector is used to label each line. If \code{NULL}, nothing is plotted above the marginal probability plot.}
  \item{...}{Extra arguments passed to the \code{par} function when plotting.}
}
\value{The result of the \code{bamboo.estimate} function is a list.  Some of the more interesting elements of the list are:
  \item{mpState}{The estimated secondary structure state using the marginal probability (MP) method that selects the most likely block form for each position.}
  \item{mapState}{The estimated secondary structure state using the maximum a posterior (MAP) method that returns the visited state that maximizes the posterior probability.}
  \item{marginalProbabilities}{A matrix of marginal probabilities of each state for each position.}
}
\references{Q. Li, D. B. Dahl, M. Vannucci, H. Joo, J. W. Tsai (2014), Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction, PLOS ONE, 9(10), e109832. <DOI:10.1371/journal.pone.0109832>}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\examples{
data(bamboo.training,
     bamboo.validation.casp9,
     bamboo.validation.astral30,
     bamboo.MSA.casp9,
     bamboo.MSA.astral30)

\dontrun{
## Be patient, this example can take several seconds on a fast computer.
likelihood <- bamboo.likelihood(bamboo.training[,"primary"],bamboo.training[,"hetc"],force=TRUE)

prior.NonInfo <- bamboo.priorNonInfo()
bamboo.MSA <- c(bamboo.MSA.casp9,bamboo.MSA.astral30)

target <- "f3rvca_0"
aa <- bamboo.validation.astral30[bamboo.validation.astral30$name==target,"primary"]
fm.NonInfo <- bamboo.estimate(likelihood(aa),prior.NonInfo,5000,500)
fm.MSA     <- bamboo.estimate(likelihood(aa),bamboo.priorMSA(bamboo.MSA[[target]]),5000,500)

ss <- c(
  "Truth"=bamboo.validation.astral30[bamboo.validation.astral30$name==target,"hetc"],
  "NonInfo-MP"=fm.NonInfo$mpState,
  "MSA-MP"=fm.MSA$mpState
)
plot(fm.MSA,ss)
}
}

