\name{bamboo.training}
\docType{data}
\alias{bamboo.training}
\title{Training Dataset}
\description{
This training dataset gives the names, the primary structure (amino acid sequences), and the secondary structure of 15,201 individual proteins from the ASTRAL SCOP 1.75 structure set filtered at 95\% sequence identity as used in the paper cited below.}
\usage{bamboo.training}
\format{A data frame containing 15,201 observations on the following 3 variables.
\enumerate{
\item name: protein name;
\item primary: protein primary structure (amino acid sequence) in 20 letters denoting the 20 amino acids;
\item hetc: secondary structure in 4 letters denoting the 4 structure types: helix (H), strand (E), turn (T) and coil (C).
}}
\source{Chandonia JM, Hon G, Walker NS, Conte LL, Koehl P, et al. (2004) The astral compendium in 2004. \emph{Nucleic Acids Research} 32: D189-D192}
\references{Q. Li, D. B. Dahl, M. Vannucci, H. Joo, J. W. Tsai (2014), Bayesian Model of Protein Primary Sequence for Secondary Structure Prediction, PLOS ONE, 9(10), e109832. <DOI:10.1371/journal.pone.0109832>}
\keyword{datasets}
