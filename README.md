
<!-- README.md is generated from README.Rmd. Please edit that file -->

# colorpath

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/colorpath)](https://CRAN.R-project.org/package=colorpath)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/ijlyttle/colorpath/workflows/R-CMD-check/badge.svg)](https://github.com/ijlyttle/colorpath/actions)
<!-- badges: end -->

**Note**: on the off-chance that someone is reading this, as of
late-2020 I am reworking the entire package. Functions that are part of
the “new regime” begin with `pth_`; we can expect other functions to be
removed in time.

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

## Acknowledgments

This package rests squarely on the foundation laid by the
[**colorspace**
package](https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html),
and was inspired by [Achim Zeileis’
talk](https://www.youtube.com/watch?v=6bv2IAcNE_c) at UseR! 2019 in
Toulouse.

As well, a lot of inspiration is drawn from the design of the
[matplotlib default
colormaps](https://www.youtube.com/watch?v=xAoljeRJ3lU) presented at
PyCon 2015.

## Code of Conduct

Please note that the colorpath project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
