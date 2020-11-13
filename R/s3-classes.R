#' S3 classes
#'
#' This package uses S3 classes to help clarify expectations for
#' function arguments and return values.
#'
#' **`cpath_pal_luv`**
#'
#' A function that:
#'
#'  - takes a vector numeric argument `x`, each value expected to be
#'    between 0 and 1
#'
#'  - returns a matrix with a row for each value of `x`,
#'    and three columns: `l`, `u`, `v`
#'
#' Philosophically, this acts just like a continuous-palette function,
#' except that `LUV` values are returned instead of hex-codes.
#'
#' **`cpath_rescaler`**
#'
#' A function that:
#'
#'  - takes a numerical input `x` (vector, each value between 0 and 1)
#'  - returns a rescaled numerical vector (call it `y`),
#'    each member between 0 and 1
#'
#' There shall be a monotonic relationship between `x` and `y`.
#'
#' **`cpath_surface_hl`**
#'
#' A function that:
#'
#'  - takes a numeric input `lum` (vector, each value between 0 and 100)
#'  - returns values for hue (degrees)
#'
#' **`pth_mat`**
#'
#' A numeric matrix with:
#'  - three columns, corresponding to luminance, blue-yellow, green-red.
#'  - one row for each color.
#'  - an additional class to identify the color space.
#'
#' Associated classes:
#'
#' - **`pth_cielab`** uses CIELAB space.
#' - **`pth_cieluv`** uses CIELUV space.
#'
#' **`pth_hex`**
#'
#' `character`, each with six-digit lower-case hex-code
#'
#' @name colorpath_S3_classes
#'
NULL
