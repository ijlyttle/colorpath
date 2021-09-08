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
#'  - subsetting will not `drop` dimensions.
#'
#' Associated classes:
#'
#' - **`pth_cielab`** uses CIELAB space.
#' - **`pth_cieluv`** uses CIELUV space.
#' - **`pth_cam02ucs`** uses CAM02-UCS space.
#' - **`pth_cam16ucs`** uses CAM16-UCS space.
#' - **`pth_jzazbz100`** uses Jzabaz space, scaled to 100.
#'
#' **`pth_hex`**
#'
#' `character`, each with six-digit lower-case hex-code
#'
#' **`pth_palette`**
#'
#' A function that, for each numeric input (`0 <= x <= 1`), returns a color in a
#' given color space.
#'
#' Associated classes:
#'
#' - **`pth_palette_hex`** built using hex codes.
#' - **`pth_palette_path`** built using a path in the color space.
#'
#'
#' **`pth_surface`**
#'
#' A function that:
#'
#'  - takes a numeric input `lum` (vector, each value between 0 and 100)
#'  - returns values for hue (degrees)
#'
#'
#' -**`pth_trajectory`**
#'
#' A function that:
#'
#'  - takes a numeric input `x` (vector, each value between 0 and 1)
#'  - returns matrix with as many rows as values for `x`,
#'    columns for luminance, chroma
#'
#' @name colorpath_S3_classes
#'
NULL
