#' Color distance calculations
#'
#' There are two ways to make a color-distance calculation:
#' `pth_distance_euclid()`, which calculates a Euclidean distance within
#' a color space, or `pth_distance_metric()`, which uses a color metric
#' in [farver::compare_colour()] to calculate the distance. Note that
#' distances calculated using `pth_distance_metric()` may not follow the
#' triangle inequality.
#'
#' For both of these functions, you can specify two sets of colors:
#' `color_a` and `color_b`. Each of these has to be a vector of hex codes
#' (character or `pth_hex`), or a matrix-based collection of colors (`pth_mat`)
#' using a specific color space, e.g. `cieluv`.
#'
#' For both functions, the number of colors in each of `color_a` and `color_b`
#' determines the distance measurements that are returned:
#'
#' - If `color_b` is `NULL`, it returns the distances between consecutive colors
#' in `color_a`. The length of the returned vector will be one less than the
#' number of colors in `color_a`.
#' - If `color_b` has only one color, it returns the distances between each
#' color in `color_a` and the one color in `color_b`. The length of the returned
#' vector will be equal to the number of colors in `color_a`.
#' - If `color_a` and `color_b` have equal numbers of colors, it returns the
#' pairwise distances between each color in `color_a` and `color_b`. The length
#' of the returned vector will be equal to the number of colors in `color_a`
#' (and `color_b`).
#' - For any other arrangement, an error will be thrown.
#'
#' For `pth_distance_euclid()`, you can specify the color space in which you
#' wish to make the distance calculation by specifying a function that converts
#' to that color space. For example, if you want to use the Jzazbz-100 color
#' space, set `transformer` to `pth_to_jzazbz100`. You can use the `...` to
#' provide additional arguments to the `transformer` function.
#'
#' The default behavior of `transformer` depends on `color_a` and `color_b`:
#'
#' - If `color_a` and `color_b` are both hex codes, default is to use
#'   [pth_to_cieluv].
#' - If `color_a` and `color_b` are both specified using the same color space,
#'   the default is to use the *that* color space.
#' - If `color_a` and `color_b` use difference color spaces or representations,
#'   the default is not determined; you have to specify a `transformer` or an
#'   error is thrown.
#'
#' At present, the argument `non_luminance_weight` is an experiment. This gives
#' you a way to weight the perceptual distances to favor luminance. For example,
#' if you set `non_luminance_weight` to zero, it will return the differences in
#' luminance in the given color space.
#'
#' The `pth_distance_metric()` uses [farver::compare_colour()] to calculate
#' the distance between colors. These methods use metric functions; the triangle
#' inequality is not necessarily satisfied. You can specify the `method`; this is
#' passed on to [farver::compare_colour()].
#'
#' @param color_a,color_b Objects that can be coerced into colors,
#'   i.e. `pth_hex` or `pth_mat`.
#' @param transformer `function` used to transform the colors to new
#'   color space, e.g. [pth_to_cieluv].
#' @param non_luminance_weight `numeric` used to "discount" the effects of
#'   chroma and hue in the distance calculation.
#' @param ... additional arguments passed on to `transformer`.
#' @param method `character` metric used by `farver::compare_colour()`.
#'
#' @return `double` one value representing distance for each color comparison.
#'
#' @examples
#' hex <- c("#000000", "#663399", "#ffffff")
#' luv <- pth_to_cieluv(hex)
#'
#' # calculates distances between consecutive elements of `hex`,
#' # when providing `pth_hex`, the default is to use `cieluv` color space:
#' pth_distance_euclid(hex)
#'
#' # if we provide a matrix that has a color space, the distances are
#' # calculated using that color space; in this case we expect the same
#' # distances as above:
#' pth_distance_euclid(luv)
#'
#' # we can calculate both using a different color space by specifying a
#' # `transformer`; we will get a different set of distances from above,
#' # but they remain internally consistent:
#' pth_distance_euclid(hex, transformer = pth_to_cam02ucs)
#' pth_distance_euclid(luv, transformer = pth_to_cam02ucs)
#'
#' # to calculate pairwise distances, specify `color_a` and `color_b`,
#' # each with the same length:
#' pth_distance_euclid(hex[1:2], hex[2:3])
#'
#' # to calculate distances from a reference, set `color_b` to the reference:
#' pth_distance_euclid(hex, hex[1])
#'
#' # using a metric follows the same principles; the calculation uses
#' # farver::compare_colour(), you can specify the `method`:
#' pth_distance_metric(hex, method = "cie94")
#'
#' # calculate distance for adjacent colors:
#' pth_distance_metric(hex)
#'
#' # calculate pairwise distances:
#' pth_distance_metric(hex[1:2], hex[2:3])
#'
#' # calculate distance from reference:
#' pth_distance_metric(hex, hex[1])
#'
#' @export
#'
pth_distance_euclid <- function(color_a, color_b = NULL, transformer = NULL,
                                non_luminance_weight = 1, ...) {

  list_color <- get_colors(color_a, color_b)
  color_a <- list_color$color_a
  color_b <- list_color$color_b

  transformer <- transformer %||% get_transformer_default(color_a, color_b)

  assertthat::assert_that(
    is.function(transformer)
  )

  assertthat::assert_that(
    assertthat::is.number(non_luminance_weight),
    non_luminance_weight >= 0
  )

  # transform to reference color-space
  color_a <- transformer(color_a, ...)
  color_b <- transformer(color_b, ...)

  # scale the non-luminance dimensions
  color_a[, 2:3] <- color_a[, 2:3] * non_luminance_weight
  color_b[, 2:3] <- color_b[, 2:3] * non_luminance_weight

  # calculate the Euclidean distance
  distance <- (color_a - color_b)**2
  distance <- rowSums(distance)
  distance <- sqrt(distance)

  distance
}


#' @rdname pth_distance_euclid
#' @export
#'
pth_distance_metric <- function(color_a, color_b = NULL,
                                method = c("cie2000", "cie94", "cie1976", "cmc")) {

  list_color <- get_colors(color_a, color_b)
  color_a <- list_color$color_a
  color_b <- list_color$color_b

  method <- match.arg(method)

  xyz100_a <- to_xyz100(color_a)
  xyz100_b <- to_xyz100(color_b)

  distance <- rep(0, pth_n_color(color_a))

  for (i in seq_along(distance)) {

    distance[i] <-
      farver::compare_colour(
        from = xyz100_a[i, , drop = FALSE],
        to = xyz100_b[i, , drop = FALSE],
        from_space = "xyz",
        method = method
      )
  }

  distance
}

#' Determine distance colors
#'
#' - if `color_b` is `NULL`, `color_a` is the all but last color of `color_a`;
#'     `color_b` is all but first color of `color_a`.
#' - if `color_b` has length `1`, it is repeated to have the length of `color_a`.
#' - check that `color_b` has the same length as `color_a`, throw error otherwise.
#'
#' @param color_a, color_b object with class `pth_mat` or `pth_hex`
#'
#' @return `list()` with elements `color_a`, `color_b`
#'
#' @noRd
#'
get_colors <- function(color_a, color_b) {

  # make sure that these are - or can be coerced into - colors
  assertthat::assert_that(
    is_color(color_a),
    is_color(color_b)
  )

  # coerce to pth_hex, if needed
  if (is.character(color_a)) {
    color_a <- pth_new_hex(color_a)
  }

  if (is.character(color_b)) {
    color_b <- pth_new_hex(color_b)
  }

  # if color_b is not provided, set up so we look at internal
  # differences within color_a
  if (is.null(color_b)) {
    colors <- color_a
    color_a <- utils::head(colors, -1)
    color_b <- utils::tail(colors, -1)
  }

  # if color_b has only one element, repeat it so
  # it has the same number of elements as color_a
  if (identical(pth_n_color(color_b), 1L)) {

    len_a <- pth_n_color(color_a)

    if (is.character(color_b)) {
      new_hex <- rep(color_b, len_a)
      color_b <- pth_new_hex(new_hex)
    }

    if (inherits(color_b, "pth_mat")) {
      new_mat <- do.call(rbind, rep(list(color_b), len_a))
      color_b <- pth_mat_replace_data(color_b, new_mat)
    }
  }

  # finally, assert that color_a and color_b have the same length
  assertthat::assert_that(
    pth_n_color(color_a) == pth_n_color(color_b),
    msg = "length of `color_a` and `color_b` not compatible"
  )

  # result
  list(color_a = color_a, color_b =  color_b)
}

# operates on the entire structure, not vectorized
is_color <- function(x) {
  is_mat(x) || all(is_hex_liberal(x))
}


#' Get default transformer-function
#'
#' @inheritParams get_colors
#'
#' Call this *after* get_colors() as it will take care of validating and
#' coercing inputs.
#'
#' The point here is to return a default transformation-function
#'
#' - if `color_a` and `color_b` are both hex-codes, we use `pth_to_cieluv()`.
#' - if `color_a` and `color_b` both use the same color space, we use `identity()`.
#' - if `color_a` and `color_b` use different color spaces, return `NULL`.
#'
#' The calling function will have to throw an error if the transformer function
#' ends up `NULL`.
#'
#' @return transformer `function`
#'
#' @noRd
#'
get_transformer_default <- function(color_a, color_b) {

  if (!identical(class(color_a), class(color_b))) {
    return(NULL)
  }

  if (inherits(color_a, "pth_hex")) {
    return(pth_to_cieluv)
  }

  if (inherits(color_a, "pth_mat"))  {
    return(identity)
  }

  NULL
}
