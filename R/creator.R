#' Extract creator function
#'
#' @inheritParams pth_transformer
#'
#' @return `function` that encapsulates a creator, i.e. `pth_new_cieluv`
#'
#' @export
#'
pth_creator <- function(mat, ...) {
  UseMethod("pth_creator")
}

#' @rdname pth_creator
#' @export
#'
pth_creator.default <- function(mat, ...) {
  stop(
    glue::glue("No method for class {class(mat)}")
  )
}

#' @export
#'
`[.pth_mat` <- function(x, i, ...) {

  # we need this so that when we subset, the rest of the
  # attributes "come along for the ride"

  # subset normally, don't drop dimensions
  mat <- NextMethod(drop = FALSE)

  # if we don't have three columns, no classes, no attributes
  if (!identical(ncol(mat), 3L)) {
    return(mat)
  }

  creator <- pth_creator(x)

  creator(mat)
}
