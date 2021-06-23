#' Get colorspace name
#'
#' @param x `object` for which we want the colorspace name.
#'   Can be a color matrix, surface, or palette.
#' @param ... other args, not used.
#'
#' @return `character` name of the colorspace, e.g. `"CIELUV"`
#'
#' @export
#'
pth_colorspace_name <- function(x, ...) {
  UseMethod("pth_colorspace_name")
}

#' @export
#'
pth_colorspace_name.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @export
#'
pth_colorspace_name.pth_surface <- function(x, ...) {
  pth_colorspace_name(x$colors)
}
