#' Replace data in `pth_mat`
#'
#' @inheritParams pth_to_cielab
#' @param new_data `matrix` with 3 columns, new data for `mat`
#' @param ... other args (not used)
#'
#' @return Object with same class, attributes as `mat`
#' @export
#'
pth_mat_replace_data <- function(mat, ...) {
  UseMethod("pth_mat_replace_data")
}

#' @rdname pth_mat_replace_data
#' @export
#'
pth_mat_replace_data.default <- function(mat, ...) {
  stop(
    glue::glue("No method for class {class(mat)}")
  )
}

#' @rdname pth_mat_replace_data
#' @export
#'
pth_mat_replace_data.pth_mat <- function(mat, new_data, ...) {

  # check the "new" matrix is suitable
  assertthat::assert_that(
    is_mat(new_data)
  )

  # get the attributes from the "old" matrix
  atts <- attributes(mat)
  atts$dim <- NULL

  # create the new matrix
  do.call(structure, c(list(.Data = new_data), atts))
}
