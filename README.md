
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StatCompLab

<!-- badges: start -->
<!-- badges: end -->

This package collects workshop materials for Statistical Computing
(MATH10093) at the University of Edinburgh 2021/22.

The tutorial documents are browsable online at
<https://finnlindgren.github.io/StatCompLab/>

Contact Finn Lindgren, <finn.lindgren@ed.ac.uk> for information.

The package version number system is
`YearOfStudy`.`StudyWeek`.`MinorUpdate`

## Installation

You can install (and later upgrade) the package from
[GitHub](https://github.com/) with:

``` r
# If devtools isn't installed, first run install.packages("devtools")
devtools::install_github("finnlindgren/StatCompLab", build_vignettes = TRUE)
```

## Example

The most convenient way to access the tutorial documents is to run them
in the `Tutorial` tab in the upper right part of RStudio. That allows
you to see the tutorial text and code hints at the same as your code
files, and while running code in the Console and viewing plots.

These alternative methods also allows viewing of the documents, but are
less convenient; vignettes show up in the same space as the plots, and
the `run_tutorial` blocks the Console window from being used to run
code.

``` r
library(StatCompLab)
vignette(package = "StatCompLab") # List available vignettes
vignette("Tutorial01", "StatCompLab") # View a specific vignette
library(learnr)
available_tutorials("StatCompLab") # List available tutorials
run_tutorial("Tutorial01", "StatCompLab") # Run a specific tutorial
```
