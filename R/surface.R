#' Create luminance-hue surface
#'
#' @param colors `character` hex-codes, or matrix with object with
#'   S3 class `pth_mat`. Must have one or two elements.
#' @param route `character`, indicates which direction to take around the
#'   circle; must be either `"short"` (default), or `"long"`.
#' @param transformer `function` used to transform to a color space.
#'   Used if `colors` are hex-codes.
#' @param n_step (`double` coerced to) `integer` number of steps to take along
#'   luminance domain to calculate maximum chroma.
#' @param ... other args, not used
#'
#' @return Onject with S3 class `pth_new_surface`.
#'
#' @export
#'
pth_new_surface <- function(colors, route = c("short", "long"), n_step = 30,
                            ...) {
  UseMethod("pth_new_surface")
}

#' @rdname pth_new_surface
#' @export
#'
pth_new_surface.default <- function(colors, route = c("short", "long"),
                                    n_step = 30, ...) {
  stop(
    glue::glue("No method for class {class(colors)}")
  )
}

#' @rdname pth_new_surface
#' @export
#'
pth_new_surface.character <- function(colors, route = c("short", "long"),
                                      n_step = 30, transformer = pth_to_cieluv,
                                      ...) {

  # transform to color space, call again
  mat <- transformer(colors)

  pth_new_surface(mat, route = route, ...)
}

pth_new_surface.pth_mat <- function(colors, route = c("short", "long"),
                                    n_step = 30, ...) {

  # verify that there are one or two colors
  assertthat::assert_that(
    length(colors[, 1]) %in% c(1L, 2L),
    msg = "A surface is defined using one or two colors."
  )

  route <- match.arg(route)

  # get angles
  mat_polar <- pth_to_polar(colors)

  luminance <- mat_polar[, 1]
  hue <- mat_polar[, 3]

  # make a proper domain and range
  if (identical(length(luminance), 1L)) {
    luminance <- c(0, 100)
  }

  hue <- get_angles(hue, route)

  # create function for hue
  fn_hue <- function(lum) {

    x <- (lum - luminance[1]) / (luminance[2] - luminance[1])

    y <- hue[1] + x * (hue[2] - hue[1])

    y # do we need to limit this to [0, 360]?
  }

  # create function for maximum chroma

  # create dummy colors
  max_chr_luminance <- seq(0, 100, length.out = n_step + 1)
  max_chr_chroma <- rep(20, n_step + 1)
  max_chr_hue <- fn_hue(max_chr_luminance)

  # create matrix for dummy colors
  mat_polar <-
    matrix(
      c(max_chr_luminance, max_chr_chroma, max_chr_hue),
      ncol = 3,
      byrow = FALSE
    )

  mat_cart <- pth_to_cartesian(mat_polar)

  # put these coordinates into its color space
  mat_space <- pth_creator(colors)(mat_cart)

  # get max chroma for each of these colors
  max_chroma <- pth_max_chroma(mat_space)

  fn_max_chroma <-
    stats::approxfun(max_chr_luminance, max_chroma, yleft = 0, yright = 0)

  # return structure (hue function, max-chroma function, colors)
  structure(
    list(
      fn_hue = fn_hue,
      fn_max_chroma = fn_max_chroma,
      colors = colors
    ),
    class = ("pth_surface")
  )
}
