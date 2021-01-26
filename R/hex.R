#' Convert to hex code
#'
#' @param color `character` or `matrix` with S3 class `pth_mat`, a
#'   representation of a color.
#' @param ..., other arguments as may be needed.
#'
#' @return `character` with S3 class `pth_hex`
#' @examples
#'   pth_to_hex("#663399")
#'   pth_to_hex(c("#221133", "#442266", "#663399"))
#' @export
#'
pth_to_hex <- function(color, ...) {
  UseMethod("pth_to_hex")
}

#' @rdname pth_to_hex
#' @export
#'
pth_to_hex.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @rdname pth_to_hex
#' @export
#'
pth_to_hex.pth_hex <- function(color, ...) {
  # no op
  color
}

#' @rdname pth_to_hex
#' @export
#'
pth_to_hex.character <- function(color, ...) {
  # coerce to pth_hex
  color <- pth_new_hex(color)

  pth_to_hex(color, ...)
}


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

  pth_hex(hex)
}

# non-validating constructor
pth_hex <- function(hex) {
  structure(
    hex,
    class = "pth_hex"
  )
}

pth_to_hex <- function(color, ...) {
  xyz <- to_xyz100(color, ...)
  hex <- farver::encode_colour(xyz, from = "xyz")

  pth_new_hex(hex)
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

#' @export
#'
to_xyz100.character <- function(color, ...) {
  color <- pth_new_hex(color)
  to_xyz100(color)
}

#' @export
#'
to_xyz100.pth_hex <- function(color, ...) {
  farver::decode_colour(color, to = "xyz")
}

#' @export
#'
`[.pth_hex` <- function(x, i, ...) {
  pth_hex(NextMethod())
}
