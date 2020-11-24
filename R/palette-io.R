#' Create palette-like functions with different inputs, outputs
#'
#' @description
#' Use these functions to create palette-like functions, but with different
#' inputs or outputs.
#'
#' Use `pth_pal_input_discrete()` to create a function
#' that takes an integer as input, rather than a numeric vector.
#' In other words, it takes a continuous-palette function
#' and returns a discrete-palette function.
#'
#' Use `pth_pal_output_hex()` to create a function that returns hex-codes,
#' rather than a matrix. In addition to converting to hex-codes, the returned
#' will "rescue" out-of-gamut colors by reducing the chroma of such colors to
#' bring them inside the gamut.
#'
#' Note that these functions do not require a `pth_palette` function, nor does
#' it return one. This means that these functions take you "off the boat".
#' Accordingly, you should consider using them as final steps.
#'
#' @param pal `function` palette-like function, returns a color
#'
#' @return \describe{
#'   \item{`pth_pal_input_discrete()`}{
#'     `function` that takes an integer as an input, returns that many colors.}
#'   \item{`pth_pal_output_hex()`}{`function` that returns hex-codes.}
#' }
#'
#' @examples
#'   hex_blue <- c("#e2e2e2", "#9cbaee", "#3c79c0")
#'
#'   pal_blue <- pth_new_palette_hex(hex_blue)
#'   pal_blue_discrete <- pth_pal_input_discrete(pal_blue)
#'
#'   pal_blue_hex <- pth_pal_output_hex(pal_blue)
#'   pal_blue_discrete_hex <- pth_pal_input_discrete(pal_blue_hex)
#'
#'   # returns matrix
#'   pal_blue(seq(0, 1, by = 0.25))
#'
#'   # takes an integer input, returns matrix
#'   pal_blue_discrete(5)
#'
#'   # returns hex-codes
#'   pal_blue_hex(seq(0, 1, by = 0.25))
#'
#'   # takes an integer input, returns hex-codes
#'   pal_blue_discrete_hex(5)
#'
#' @export
#'
pth_pal_input_discrete <- function(pal) {

  assertthat::assert_that(
    is.function(pal)
  )

  function(n) {

    assertthat::assert_that(
      assertthat::is.number(n),
      n > 0
    )

    x <- seq(0, 1, length.out = n)

    pal(x)
  }
}

#' @rdname pth_pal_input_discrete
#' @export
#'
pth_pal_output_hex <- function(pal) {

  assertthat::assert_that(
    is.function(pal)
  )

  function(x) {
    pal(x) %>%
      pth_clip_chroma() %>%
      pth_to_hex()
  }
}
