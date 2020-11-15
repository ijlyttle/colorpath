#' Access colorio whitepoints
#'
#' @param name `character` a legal name of a whitepoint:
#'   `"A"`, `"C"`, `"D50"`, `"D55"`, `"D65"`, `"D75"`
#'
#' @return `numeric` `array` with one dimension of length 3.
#' @examples
#'   # not run because it requires Python
#'   if (FALSE) {
#'     whitepoints_cie1931("D65")
#'   }
#' @export
#'
whitepoints_cie1931 <- function(name) {

  names <- names(colorio$illuminants$whitepoints_cie1931)

  assertthat::assert_that(
    name %in% names,
    msg =
      glue::glue(
        "`{name}` not a legal name: ",
        "{glue::glue_collapse(names, sep = ', ')}"
      )
  )

  colorio$illuminants$whitepoints_cie1931[[name]]
}
