#' HCL data frame
#'
#' A dataset containing values describing colors along a sample colorpath, using
#' the HCL colorspace. Describes the same colors as [mat_luv].
#'
#' @format `data.frame` with 3 rows and 3 variables:
#' \describe{
#'   \item{h}{`numeric`, hue}
#'   \item{c}{`numeric`, chroma}
#'   \item{l}{`numeric`, luminance}
#' }
#'
"df_hcl"

#' LUV matrix
#'
#' A dataset containing values describing colors along a sample colorpath, using
#' the LUV colorspace.  Describes the same colors as [df_hcl].
#'
#' @format `matrix` with 3 rows and 3 variables:
#' \describe{
#'   \item{l}{`numeric`, luminance}
#'   \item{u}{`numeric`, u component}
#'   \item{v}{`numeric`, v component}
#' }
#'
"mat_luv"
