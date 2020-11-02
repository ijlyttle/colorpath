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
#'  - takes a numerical input `x` (vector, each member between 0 and 1)
#'  - returns a rescaled numerical vector (call it `y`),
#'    each member between 0 and 1
#'
#' There shall be a monotonic relationship between `x` and `y`.
#'
#' **`cpath_surface_hl`**
#'
#' A function that:
#'
#'  - takes a numeric input `lum` (vector, each memeber between 0 and 100)
#'  - returns values for hue
#'
#' **`pth_mat`**
#'
#' A numeric matrix with three columns, and one row for each color.
#'
#'  - has an attribute `color_space` to identify which color space it uses.
#'
#' @name colorpath_S3_classes
#'
NULL
