# reference: https://rstudio.github.io/reticulate/articles/package.html

#' colorio object
#'
#' Uses the reticulate framework to access the colorio API.
#'
#' The colorio Python package is exposed through the `colorio` object.
#'
#' In this package, use the `$` operator wherever you see the `.` operator
#' used in Python.
#'
#' @noRd
#'
colorio <- NULL

.onLoad <- function(libname, pkgname) {

  colorio <<- colorio::colorio

}
