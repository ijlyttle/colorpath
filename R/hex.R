#' Coerce to hex code
#'
#' Use to validate and coerce to character-based hex-codes.
#' We coerce to CSS six-digit hex codes that use lower-case,
#' e.g. `"#336699"`, `"#aabbcc"`.
#'
#' @param hex `character` that can be coerced to a six-digit hex code.
#'
#' @return `character`, six-character hex code.
#' @examples
#'   pth_new_hex("#11223344")
#'   pth_new_hex("#AA1122")
#' @export
#'
pth_new_hex <- function(hex) {

  assertthat::assert_that(
    all(is_hex_liberal(hex)),
    msg = "`hex` must be a six-digit or eight-digit hex-code"
  )

  hex <- as_hex(hex)

  return(hex)
}

# liberal - accepts 6 or 8 digits
is_hex_liberal <- function(x) {
  grepl("^#[A-Fa-f0-9]{2}{3,4}$", x)
}

# conservative - return only 6 digits
as_hex <- function(x) {
  x <- sub("^(#[A-Fa-f0-9]{6})[A-Fa-f0-9]{2}$", "\\1", x)
  x <- tolower(x)

  x
}
