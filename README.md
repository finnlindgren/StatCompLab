
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StatCompLab

<!-- badges: start -->
<!-- badges: end -->

This package collects workshop materials for Statistical Computing
(MATH10093) at the University of Edinburgh 2020/21.

Contact Finn Lindgren, <finn.lindgren@ed.ac.uk> for information.

## Installation

You can install (and later upgrade) the package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("finnlindgren/StatCompLab", build_vignettes = TRUE)
```

## Example

``` r
library(StatCompLab)
vignette("Lab01", "StatCompLab")
library(learnr)
run_tutorial("Tutorial01", "StatCompLab")
```
