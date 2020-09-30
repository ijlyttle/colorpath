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
