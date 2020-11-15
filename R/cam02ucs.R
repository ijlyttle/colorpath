#' Convert/coerce to CAM02-UCS matrix
#'
#' @inheritParams pth_to_cielab
#' @param c `numeric` surround parameter, between 0.535 (dark) and 0.69 (average).
#' @param Y_b `numeric` background luminance.
#' @param L_A `numeric` luminance of the adapting field (cd/m^2).
#'
#' @return `double` `matrix` with S3 classes `pth_cam02ucs` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_cam02ucs("#663399")
#'   pth_new_cam02ucs(matrix(c(35.5, 13.4, -24.2), ncol = 3))
#' @export
#'
pth_to_cam02ucs <- function(color, c = 0.69, Y_b = 20, L_A = 64 / pi / 5,
                            whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cam02ucs <-
    colorio$CAM02(
      variant = "UCS",
      c = c,
      Y_b = Y_b,
      L_A = L_A,
      whitepoint = whitepoint
    )

  # get values
  xyz <- to_xyz100(color)
  mat <- t(cam02ucs$from_xyz100(t(xyz)))

  pth_new_cam02ucs(mat, c = c, Y_b = Y_b, L_A = L_A, whitepoint = whitepoint)
}

#' @rdname pth_to_cam02ucs
#' @export
#'
pth_new_cam02ucs <- function(mat, c = 0.69, Y_b = 20, L_A = 64 / pi / 5,
                             whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cam02ucs <-
    colorio$CAM02(
      variant = "UCS",
      c = c,
      Y_b = Y_b,
      L_A = L_A,
      whitepoint = whitepoint
    )

  # save whitepoint as attribute
  result <- structure(mat, class = c("pth_cam02ucs", "pth_mat"))
  attr(result, "whitepoint") <- whitepoint
  attr(result, "c") <- c
  attr(result, "Y_b") <- Y_b
  attr(result, "L_A") <- L_A

  # attach labels
  result <- label_cols(result, cam02ucs$labels)

  result
}

#' @export
#'
to_xyz100.pth_cam02ucs <- function(color, ...) {

  cam02ucs <-
    colorio$CAM02(
      variant = "UCS",
      c = attr(color, "c"),
      Y_b = attr(color, "Y_b"),
      L_A = attr(color, "L_A"),
      whitepoint = attr(color, "whitepoint")
    )

  xyz100 <- t(cam02ucs$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}
