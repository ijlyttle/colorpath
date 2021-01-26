#' Extract transformer function
#'
#' @param mat matrix with object with S3 class `pth_mat`
#' @param ... other args, not used
#'
#' @return `function` that encapsulates a transformer, i.e. `pth_to_cieluv`
#'
#' @export
#'
pth_transformer <- function(mat, ...) {
  UseMethod("pth_transformer")
}

pth_transformer.default <- function(mat, ...) {
  stop(
    glue::glue("No method for class {class(mat)}")
  )
}
