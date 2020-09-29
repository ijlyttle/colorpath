#' Convert to LUV matrix from HCL data frame
#'
#' It is thought to specify a colorpath using HCL coordinates;
#' it seems reasonable to compile these coordinates using a
#' data frame.
#'
#' @param df_hcl `data.frame` with variables `h`, `c`, `l`
#'
#' @return `matrix` with columns, `l`, `u`, `v` and same number
#'   of rows as `df_hcl`
#'
#' @examples
#'   as_mat_luv(df_hcl_blues)
#' @export
#'
as_mat_luv <- function(df_hcl) {

  hcl_mat <- as.matrix(df_hcl[, c("h", "c", "l")])

  farver::convert_colour(hcl_mat, from = "hcl", to = "luv")
}

