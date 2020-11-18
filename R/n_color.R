#' Number of colors
#'
#' @inheritParams pth_to_cielab
#' @param ... other args (not used)
#'
#' @return `integer` number of colors
#' @examples
#'   pth_n_color(c("#000000", "#663399", "#FFFFFF"))
#' @export
#'
pth_n_color <- function(color, ...) {
  UseMethod("pth_n_color")
}

#' @rdname pth_n_color
#' @export
#'
pth_n_color.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @rdname pth_n_color
#' @export
#'
pth_n_color.character <- function(color, ...) {
  hex <- pth_new_hex(color)
  pth_n_color(hex)
}

#' @rdname pth_n_color
#' @export
#'
pth_n_color.pth_hex <- function(color, ...) {
  length(color)
}

#' @rdname pth_n_color
#' @export
#'
pth_n_color.pth_mat <- function(color, ...) {
  nrow(color)
}
