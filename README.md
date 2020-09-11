
<!-- README.md is generated from README.Rmd. Please edit that file -->

# colorpath

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of colorpath is to introduce as a level of abstraction above a
color palette. It is thought that a set of palettes: categorical,
sequential, and diverging, could be built using colors contained within
a set of paths. A color path is built by constructing splines through
HCL space, then scaling the input to aim for perceptual uniformity.

This may also be useful for constructing sets of palettes each for light
mode and dark mode.

## Installation

You can install the github version of colorpath from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("remotes")
remotes::install_github("ijlyttle/colorpath")
```
