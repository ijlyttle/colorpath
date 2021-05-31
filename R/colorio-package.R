#' Check colorio
#'
#' @return Invisible NULL, called for side effects.
#' @export
#'
check_colorio <- function() {

  vers <- colorio::colorio_version()
  vers_req <- "0.7.3"

  if (utils::compareVersion(vers, vers_req) < 0) {
    warning(
      glue::glue(
        "Current colorio version is {vers}; {vers_req} is required. ",
        "Use colorio::install_colorio()."
      )
    )
  } else {
    message(glue::glue("colorio version, {vers}, is good."))
  }

  invisible(NULL)
}
