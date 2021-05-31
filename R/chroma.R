#' Get "distance" from gamut surface
#'
#' @inheritParams pth_to_hex
#'
#' @return `double` for each color: positive indicates out-of-gamut,
#'   negative or zero indicates in-gamut
#'
#' @noRd
#'
x_gamut <- function(color) {

  # sometimes, out-of-gamut throws an error. instead of an error, we give a value.
  result <- 100

  try(
    {
      rgb <- to_rgb(color)
      dimnames(rgb) <- NULL

      # positive outside of 0 <= x <= 255
      .f <- function(x) {
        result <- abs(x - 127.5) - 127.5

        result[is.na(result) | is.nan(result)] <- 1

        result
      }

      x <- .f(rgb)

      result <- pmax(x[, 1], x[, 2], x[, 3])
    },
    silent = TRUE
  )

  result
}

#' Determine if color is in RGB gamut
#'
#' @inheritParams pth_to_cielab
#'
#' @return `logical` for each color indicates if in-gamut
#' @examples
#'   pth_in_gamut("#663399")
#' @export
#'
pth_in_gamut <- function(color) {
  x_gamut(color) <= 0
}

#' Determine maximum chroma
#'
#' The chroma is calculated given the color space used in `mat`.
#'
#' @inheritParams pth_to_cielab
#'
#' @return `double` maximum chroma, one value for each row in `mat`.
#' @export
#'
pth_max_chroma <- function(mat) {

  assertthat::assert_that(
    inherits(mat, "pth_mat")
  )

  len <- nrow(mat)

  if (identical(len, 0L)) {
    return(NULL)
  }

  # for loop out of necessity
  max_chroma <- rep(double(0), len)
  if (interactive()) {
    pb <- progress::progress_bar$new(total = len)
  }
  for (i in seq(len)) {
    mat_local <- mat[i, ]
    max_chroma[i] <- root_chroma(mat_local)

    if (interactive()) {
      pb$tick()
    }
  }

  max_chroma
}


#' Modify color to bring inside RGB gamut
#'
#' @inheritParams pth_to_cielab
#' @param ... other args (not used).
#'
#' @return Object of same type as `color`.
#' @export
#'
pth_clip_chroma <- function(color, ...) {
  UseMethod("pth_clip_chroma")
}

#' @export
#'
pth_clip_chroma.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
pth_clip_chroma.character <- function(color, ...) {
  hex <- pth_to_hex(color)
  pth_clip_chroma(hex)
}

#' @export
#'
pth_clip_chroma.pth_hex <- function(color, ...) {
  # no-op
  color
}

#' @export
#'
pth_clip_chroma.pth_mat <- function(color, ...) {

  in_gamut <- pth_in_gamut(color)

  # get chroma and mat chroma for all colors not in gamut
  chroma <- pth_to_polar(color[!in_gamut, ])[, 2]
  max_chroma <- pth_max_chroma(color[!in_gamut, ])

  # desaturate colors not in gamut
  ratio <- max_chroma / chroma
  color[!in_gamut, 2:3] <- color[!in_gamut, 2:3] * ratio

  color
}

#' Determine gamut-distance for a chroma, given a color
#'
#' @param chroma `numeric` value for chroma, expressed using color space
#'   for `mat`.
#' @inheritParams pth_to_hex
#'
#' @return `double` indicating "distance" from gamut surface,
#'   negative indicates inside gamut.
#'
#' @keywords internal
#' @export
#'
x_gamut_chroma <- function(chroma, mat) {

  # TODO: potential to memoise
  polar <- pth_to_polar(mat)

  polar[, 2] <- chroma
  mat_new <- pth_to_cartesian(polar, chroma_min = 0)

  mat[, 2:3] <- mat_new[, 2:3]

  x_gamut(mat)
}

#' Get single root
#'
#' @param mat `matrix` with S3 class `pth_mat` and **exactly** one row.
#'
#' @return `double` maximum chroma value for RGB gamut, given luminance and
#'   hue in `mat`.
#'
#' @noRd
#'
root_chroma <- memoise::memoise(function(mat, tol = 0.1) {

  # short-circuit the top and bottom of the gamut
  lum <- mat[, 1]
  tol_lum <- 0.9
  if (abs(lum) < tol_lum || abs(100 - lum) < tol_lum) {
    return(0) # max_chroma is zero
  }

  root <-
    stats::uniroot(
      x_gamut_chroma,
      interval = chroma_interval(),
      mat = mat,
      extendInt = "yes",
      tol = tol
    )

  result <- root$root

  options(colorpath.max.chroma = result)

  result
})

chroma_interval <- function(default = c(0, 1)) {

  # use option to find the last value calculated
  last_value <- getOption("colorpath.max.chroma", 0)

  if (last_value < 1) {
    return(default)
  }

  last_value * c(0.9, 1.1)
}
