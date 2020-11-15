#' Convert/coerce to JzAzBz-100 matrix
#'
#' The idea for the scaling came from
#' https://github.com/nschloe/colorio/issues/41#issuecomment-519983981.
#'
#' @inheritParams pth_to_cielab
#'
#' @return `double` `matrix` with S3 classes `pth_jzazbz100` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_jzazbz100("#663399")
#'   pth_new_jzazbz100(matrix(c(36.3, 15,3, -42.6), ncol = 3))
#' @export
#'
pth_to_jzazbz100 <- function(color, whitepoint = whitepoints_cie1931("D65")) {

  # use internal function
  mat <- pth_to_jzazbz(color, whitepoint = whitepoint)

  # scale values
  mat <- mat * jzazbz_scale100(whitepoint = whitepoints_cie1931("D65"))

  pth_new_jzazbz100(mat, whitepoint = whitepoint)
}

# internal function
pth_to_jzazbz <- function(color, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  jzazbz <- colorio$JzAzBz(whitepoint = whitepoint)

  # get values
  xyz <- to_xyz100(color)
  mat <- t(jzazbz$from_xyz100(t(xyz)))

  mat
}

#' @rdname pth_to_jzazbz100
#' @export
#'
pth_new_jzazbz100 <- function(mat, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  jzazbz100 <- colorio$JzAzBz(whitepoint = whitepoint)

  # save whitepoint as attribute
  result <- structure(mat, class = c("pth_jzazbz100", "pth_mat"))
  attr(result, "whitepoint") <- whitepoint

  # attach labels
  result <- label_cols(result, jzazbz100$labels)

  result
}

#' @export
#'
to_xyz100.pth_jzazbz100 <- function(color, ...) {

  jzazbz100 <- colorio$JzAzBz(whitepoint = attr(color, "whitepoint"))

  # unscale color
  color <- color / jzazbz_scale100(whitepoint = whitepoints_cie1931("D65"))

  xyz100 <- t(jzazbz100$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}

# we are going to scale using luminance only
jzazbz_scale100 <- function(whitepoint = whitepoints_cie1931("D65")) {

  white <- pth_to_jzazbz("#FFFFFF")
  # black is at the origin

  100 / white[[1]]
}


