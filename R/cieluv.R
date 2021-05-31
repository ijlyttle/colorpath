#' Convert/coerce to CIELUV matrix
#'
#' @inheritParams pth_to_cielab
#'
#' @return `double` `matrix` with S3 classes `pth_cieluv` and `pth_mat`,
#'   with three columns, one row for each color.
#' @examples
#'   pth_to_cieluv("#663399")
#'   pth_new_cieluv(matrix(c(32.9, 13.0, -67.8), ncol = 3))
#' @export
#'
pth_to_cieluv <- function(color, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cieluv <- colorio$cs$CIELUV(whitepoint = whitepoint)

  # get values
  xyz <- to_xyz100(color)

  # add some "fuzz" to origin to get around divide-by-zero error
  # in Python code
  is_origin <- rowSums(xyz) == 0 # detect origin
  xyz[is_origin, 1] <- 1.e-10 # add fuzz

  mat <- t(cieluv$from_xyz100(t(xyz)))

  pth_new_cieluv(mat, whitepoint = whitepoint)
}

#' @rdname pth_to_cieluv
#' @export
#'
pth_new_cieluv <- function(mat, whitepoint = whitepoints_cie1931("D65")) {

  # establish color space
  cieluv <- colorio$cs$CIELUV(whitepoint = whitepoint)

  # save whitepoint as attribute
  result <-
    structure(
      mat,
      class = c("pth_cieluv", "pth_mat"),
      whitepoint = whitepoint
    )

  # attach labels
  result <- label_cols(result, cieluv$labels)

  result
}

#' @rdname pth_transformer
#' @export
#'
pth_transformer.pth_cieluv <- function(mat, ...) {

  function(color) {
    pth_to_cieluv(
      color,
      whitepoint = attr(mat, "whitepoint")
    )
  }

}

#' @rdname pth_creator
#' @export
#'
pth_creator.pth_cieluv <- function(mat, ...) {

  function(mat_new) {
    pth_new_cieluv(
      mat_new,
      whitepoint = attr(mat, "whitepoint")
    )
  }

}

#' @export
#'
to_xyz100.pth_cieluv <- function(color, ...) {

  cieluv <- colorio$cs$CIELUV(whitepoint = attr(color, "whitepoint"))

  # add some "fuzz" to origin to get around divide-by-zero error
  # in Python code
  is_origin <- color[, 1] == 0 # detect origin
  color[is_origin, 1] <- 1.e-10 # add fuzz

  xyz100 <- t(cieluv$to_xyz100(t(color)))

  label_cols(xyz100, c("x", "y", "z"))
}

