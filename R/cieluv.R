#' Convert/coerce to CIELUV matrix
#'
#' @inheritParams pth_to_hex
#' @param mat `double` `matrix` with three columns, one row for each color.
#' @param whitepoint `double` `array` with one dimension, length three;
#'   describes the whitepoint reference for the color space.
#'
#' @return `double` `matrix` with S3 classes `pth_cieluv` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_cieluv("#663399")
#'   pth_new_cieluv(matrix(c(32.9, 13.0, -67.8), ncol = 3))
#' @export
#'
pth_to_cieluv <- function(color, whitepoint = whitepoint_cie1931("D65"), ...) {

  # establish color space
  cieluv <- colorio$CIELUV(whitepoint = whitepoint)

  # get values
  xyz <- to_xyz100(color)
  mat <- t(cieluv$from_xyz100(t(xyz)))

  pth_new_cieluv(mat, whitepoint = whitepoint)
}

#' @rdname pth_to_cieluv
#' @export
#'
pth_new_cieluv <- function(mat, whitepoint = whitepoint_cie1931("D65"), ...) {

  # establish color space
  cieluv <- colorio$CIELUV(whitepoint = whitepoint)

  # save whitepoint as attribute
  result <- structure(mat, class = c("pth_cieluv", "pth_mat"))
  attr(result, "whitepoint") <- whitepoint

  # attach labels
  result <- label_cols(result, cieluv$labels)

  result
}

#' @export
#'
to_xyz100.pth_cieluv <- function(color, ...) {

  cieluv <- colorio$CIELUV(whitepoint = attr(color, "whitepoint"))

  xyz100 <- t(cieluv$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}


