#' Color-vision deficiency grid
#'
#' Helper function to generate grid for color-vision deficiency (CVD)
#' possibilities.
#'
#' @param cvd `character` one or more color-vision deficiency conditions;
#'  legal values: `"none"`, `"deutan"`, `"protan"`, `"tritan"`.
#' @param severity `double` one or more values of severity for a given
#'  condition; legal values from 0 to 1.
#'
#' @return `tbl_df` with columns `cvd` (factor), `severity` (double)
#' @export
#'
pth_cvd_grid <- function(cvd = c("none", "deutan", "protan", "tritan"),
                         severity = 1) {

  # validate inputs
  cvd <- match.arg(cvd, several.ok = TRUE)

  assertthat::assert_that(
    is.numeric(severity),
    all(severity >= 0),
    all(severity <= 1)
  )

  expand.grid(cvd = cvd, severity = severity) %>%
    tibble::as_tibble()
}

#' @rdname pth_cvd_grid
#' @export
#'
pth_cvd_grid_full <- function(cvd = c("deutan", "protan", "tritan"),
                              severity = c(0, 0.33, 0.67, 1)) {

  pth_cvd_grid(cvd = cvd, severity = severity)
}
