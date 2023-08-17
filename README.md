# bamboo: Protein Secondary Structure Prediction Using the Bamboo Method

Determining the primary structure (i.e., amino acid sequence) of a protein has become cheaper, faster, and more accurate. Higher order protein structure provides insight into a protein’s function in the cell. Understanding a protein’s secondary structure is a first step towards this goal. Therefore, a number of computational prediction methods have been developed to predict secondary structure from just the primary amino acid sequence. The most successful methods use machine learning approaches that are quite accurate, but do not directly incorporate structural information. As a step towards improving secondary structure reduction given the primary structure, we propose a Bayesian model based on the knob-socket model of protein packing in secondary structure. The method considers the packing influence of residues on the secondary structure determination, including those packed close in space but distant in sequence. By performing an assessment of our method on 2 test sets we show how incorporation of multiple sequence alignment data, similarly to PSIPRED, provides balance and improves the accuracy of the predictions. Software implementing the methods is provided as a web application and a stand-alone implementation

## Installation

In R, install the package by executing:

```R
install.packages("remotes")
remotes::install_github("dbdahl/rscala/R/rscala")
remotes::install_github("dbdahl/bamboo/R/bamboo")
```

## Resources

* [Paper](https://doi.org/10.1371/journal.pone.0109832) describes the methods implemented in this package.

```R
library(bamboo)
example(bamboo)
help(bamboo)
```


