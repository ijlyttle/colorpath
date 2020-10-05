new_pal_luv <- function(.f, spec_luv = NULL) {

  # if spec_luv not provided, see if incoming has one
  spec_luv <- spec_luv %||% spec_luv(.f)

  # build new structure
  .f <- structure(.f, class = "cpath_pal_luv")
  attr(.f, "spec_luv") <- spec_luv

  .f
}

spec_luv <- function(.f) {
  attr(.f, "spec_luv")
}

#' @export
#'
print.cpath_pal_luv <- function(x, ...) {

  cat("function(x): \n")
  cat("  input:  vector (0 <= x <= 1)\n")
  cat("  output: matrix LUV values\n\n")
  cat("  B\u00e9zier spline based on these control points:\n")
  print(round(spec_luv(x), 3))

  invisible(x)
}
