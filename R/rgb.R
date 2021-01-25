#' Convert to rgb
#'
#' Note that we are exporting the S3 methods, but not the generic.
#'
#' @noRd
#'
to_rgb <- function(color, ...) {
  UseMethod("to_rgb")
}

#' @export
#'
to_rgb.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
to_rgb.pth_hex <- function(color, ...) {
  farver::decode_colour(color, to = "rgb")
}

#' @export
#'
to_rgb.character <- function(color, ...) {
  hex <- pth_new_hex(color)
  to_rgb(hex)
}

#' @export
#'
to_rgb.pth_mat <- function(color, ...) {

  xyz100 <- to_xyz100(color)

  xyz100_to_rgb255(xyz100)
}

#' @export
#'
to_xyz100.pth_srgb255 <- function(color, ...) {

  # srgb255 == srgb == farver::rgb
  rgb255_to_xyz100(color)
}

# internal functions for RGB <-> XYZ
rgb255_to_xyz100 <- function(rgb255) {

  srgb_space <- colorio$SrgbLinear()

  xyz100 <-
    t(
      srgb_space$to_xyz100(
        srgb_space$from_srgb255(t(rgb255))
      )
    )

  xyz100
}

xyz100_to_rgb255 <- function(xyz100) {

  srgb_space <- colorio$SrgbLinear()

  rgb255 <-
    t(
      srgb_space$to_srgb255(
        srgb_space$from_xyz100(t(xyz100))
      )
    )

  dimnames(rgb255) <- list(NULL, c("r", "g", "b"))

  rgb255
}
