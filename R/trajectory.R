#' @rdname pth_new_surface
#' @export
#'
pth_new_trajectory <- function(lum, chroma) {

  # validate chroma, lum
  assertthat::assert_that(
    is.numeric(chroma),
    is.numeric(lum),
    identical(length(chroma), length(lum)),
    length(chroma) > 0,
    all(chroma >= 0)
  )

  control_points <- matrix(c(lum, chroma), ncol = 2, byrow = FALSE)
  dimnames(control_points) <- list(NULL, c("lum", "chroma"))

  # create trajectory function
  f <- function(x) {
    mat <- bezier::bezier(t = x, p = control_points)
    dimnames(mat) <- list(NULL, c("lum", "chroma"))

    mat
  }

  structure(
    f,
    class = "pth_trajectory",
    control_points = control_points
  )
}
