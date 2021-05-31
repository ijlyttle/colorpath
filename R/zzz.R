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

  # loadNamespace("colorio")
  #
  # print("here")
  # vers <- colorio::colorio_version()
  # print(vers)
  # vers_req <- "0.7.3"
  #
  # if (utils::compareVersion(vers, vers_req) < 0) {
  #   warning(
  #     glue::glue(
  #       "Current colorio version is {vers}; {vers_req} is required. ",
  #       "Use colorio::install_colorio()."
  #     )
  #   )
  # }

  colorio <<- colorio::colorio

}
