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
