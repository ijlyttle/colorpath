#' Convert/coerce to CAM16-UCS matrix
#'
#' @inheritParams pth_to_cielab
#' @param c `numeric` surround parameter, between 0.535 (dark) and 0.69 (average).
#' @param Y_b `numeric` background luminance.
#' @param L_A `numeric` luminance of the adapting field (cd/m^2).
#' @param exact_inversion `logical` indicates to use exact inversion.
#'
#' @return `double` `matrix` with S3 classes `pth_cam16ucs` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_cam16ucs("#663399")
#'   pth_new_cam16ucs(matrix(c(36.5, 18.4, -21.9), ncol = 3))
#' @export
#'
pth_to_cam16ucs <- function(color, c = 0.69, Y_b = 20, L_A = 64 / pi / 5,
                            exact_inversion = TRUE,
                            whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cam16ucs <-
    colorio$CAM16UCS(
      c = c,
      Y_b = Y_b,
      L_A = L_A,
      exact_inversion = exact_inversion,
      whitepoint = whitepoint
    )

  # get values
  xyz <- to_xyz100(color)
  mat <- t(cam16ucs$from_xyz100(t(xyz)))

  pth_new_cam16ucs(
    mat,
    c = c,
    Y_b = Y_b,
    L_A = L_A,
    exact_inversion = exact_inversion,
    whitepoint = whitepoint
  )
}

#' @rdname pth_to_cam16ucs
#' @export
#'
pth_new_cam16ucs <- function(mat, c = 0.69, Y_b = 20, L_A = 64 / pi / 5,
                             exact_inversion = TRUE,
                             whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cam16ucs <-
    colorio$CAM16UCS(
      c = c,
      Y_b = Y_b,
      L_A = L_A,
      exact_inversion = exact_inversion,
      whitepoint = whitepoint
    )

  # save whitepoint as attribute
  result <-
    structure(
      mat,
      class = c("pth_cam16ucs", "pth_mat"),
      c = c,
      Y_b = Y_b,
      L_A = L_A,
      exact_inversion = exact_inversion,
      whitepoint = whitepoint
    )

  # attach labels
  result <- label_cols(result, cam16ucs$labels)

  result
}

#' @export
#'
to_xyz100.pth_cam16ucs <- function(color, ...) {

  cam16ucs <-
    colorio$CAM16UCS(
      c = attr(color, "c"),
      Y_b = attr(color, "Y_b"),
      L_A = attr(color, "L_A"),
      exact_inversion = attr(color, "exact_inversion"),
      whitepoint = attr(color, "whitepoint")
    )

  xyz100 <- t(cam16ucs$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}

#' @export
#'
`[.pth_cam16ucs` <- function(x, i, ...) {

  # we need this so that when we subset, the rest of the
  # attributes "come along for the ride"

  # subset normally, don't drop dimensions
  mat <- NextMethod(drop = FALSE)

  # if we don't have three columns, no classes, no attributes
  if (!identical(ncol(mat), 3L)) {
    return(mat)
  }

  pth_new_cam16ucs(
    mat,
    c = attr(x, "c"),
    Y_b = attr(x, "Y_b"),
    L_A = attr(x, "L_A"),
    exact_inversion = attr(x, "exact_inversion"),
    whitepoint = attr(x, "whitepoint")
  )
}