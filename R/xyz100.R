#' Convert to xyz100
#'
#' Each class used to express a color, e.g. pth_hex, pth_cieluv,
#' will need to implement each of these xyz100-conversion methods.
#'
#' Note that we are exporting the S3 methods, but not the generic.
#'
#' @noRd
#'
to_xyz100 <- function(color, ...) {
  UseMethod("to_xyz100")
}

#' @export
#'
to_xyz100.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

