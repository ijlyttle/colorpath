#' S3 classes
#'
#' This package uses S3 classes to help clarify expectations for
#' function arguments and return values.
#'
#' **`cpath_palette_luv`**
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
NULL
