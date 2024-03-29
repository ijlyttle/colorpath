% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cam16ucs.R
\name{pth_to_cam16ucs}
\alias{pth_to_cam16ucs}
\alias{pth_new_cam16ucs}
\title{Convert/coerce to CAM16-UCS matrix}
\usage{
pth_to_cam16ucs(
  color,
  c = 0.69,
  Y_b = 20,
  L_A = 64/pi/5,
  whitepoint = whitepoints_cie1931("D65")
)

pth_new_cam16ucs(
  mat,
  c = 0.69,
  Y_b = 20,
  L_A = 64/pi/5,
  whitepoint = whitepoints_cie1931("D65")
)
}
\arguments{
\item{color}{\code{character} or \code{matrix} with S3 class \code{pth_mat}, a
representation of a color.}

\item{c}{\code{numeric} surround parameter, between 0.535 (dark) and 0.69 (average).}

\item{Y_b}{\code{numeric} background luminance.}

\item{L_A}{\code{numeric} luminance of the adapting field (cd/m^2).}

\item{whitepoint}{\code{double} \code{array} with one dimension, length three;
describes the whitepoint reference for the color space.}

\item{mat}{\code{double} \code{matrix} with three columns, one row for each color.}
}
\value{
\code{double} \code{matrix} with S3 classes \code{pth_cam16ucs} and \code{pth_mat},
with three columns, one row for each color.
}
\description{
Convert/coerce to CAM16-UCS matrix
}
\examples{
  pth_to_cam16ucs("#663399")
  pth_new_cam16ucs(matrix(c(36.5, 18.4, -21.9), ncol = 3))
}
