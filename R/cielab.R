#' Convert/coerce to CIELAB matrix
#'
#' @inheritParams pth_to_hex
#' @param mat `double` `matrix` with three columns, one row for each color.
#' @param whitepoint `double` `array` with one dimension, length three;
#'   describes the whitepoint reference for the color space.
#'
#' @return `double` `matrix` with S3 classes `pth_cielab` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_cielab("#663399")
#'   pth_new_cielab(matrix(c(32.9, 42.9, -47.1), ncol = 3))
#' @export
#'
pth_to_cielab <- function(color, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cielab <- colorio$cs$CIELAB(whitepoint = whitepoint)

  # get values
  xyz <- to_xyz100(color)
  mat <- t(cielab$from_xyz100(t(xyz)))

  pth_new_cielab(mat, whitepoint = whitepoint)
}

#' @rdname pth_to_cielab
#' @export
#'
pth_new_cielab <- function(mat, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cielab <- colorio$cs$CIELAB(whitepoint = whitepoint)

  # save whitepoint as attribute
  result <-
    structure(
      mat,
      class = c("pth_cielab", "pth_mat"),
      whitepoint = whitepoint
    )

  # attach labels
  result <- label_cols(result, cielab$labels)

  result
}

#' @rdname pth_transformer
#' @export
#'
pth_transformer.pth_cielab <- function(mat, ...) {

  function(color) {
    pth_to_cielab(
      color,
      whitepoint = attr(mat, "whitepoint")
    )
  }

}

#' @rdname pth_creator
#' @export
#'
pth_creator.pth_cielab <- function(mat, ...) {

  function(mat_new) {
    pth_new_cielab(
      mat_new,
      whitepoint = attr(mat, "whitepoint")
    )
  }

}

#' @export
#'
to_xyz100.pth_cielab <- function(color, ...) {

  cielab <- colorio$cs$CIELAB(whitepoint = attr(color, "whitepoint"))

  xyz100 <- t(cielab$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}

#' @export
#'
pth_colorspace_name.pth_cielab <- function(x, ...) {
  "CIELAB"
}
