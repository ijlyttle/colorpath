% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescaler.R
\name{rescaler_x}
\alias{rescaler_x}
\alias{rescaler_lum}
\title{Rescaler functions}
\usage{
rescaler_x(range)

rescaler_lum(range, pal_luv)
}
\arguments{
\item{range}{\code{numeric} values, in \code{x} or in luminance, to correspond with \code{x = c(0, 1)}}

\item{pal_luv}{\code{cpath_pal_luv}, palette function on which the
luminance range will operate}
}
\value{
A function with S3 class \code{cpath_rescaler}.
}
\description{
Use this function to rescale a palette function, for example: clip a certain part
of it.
}
\details{
A rescaler function:
\itemize{
\item must accept input between zero and one.
\item must provide an output between zero and one.
\item the output must to vary monotonically with the input.
}

Use \code{rescaler_linear()} create a new palette-function that uses part
of the range of an existing palette-function.

Use \code{rescaler_bezier()} to rescale a Bézier palette-function to be more
perceptually uniform. This uses fits a spline to equally-spaced points on
the colorpath. The Bézier function makes some optimization calculations,
so it may take a few seconds to run.
}
\examples{
  # Input rescaler
  rlin <- rescaler_x(c(0.25, 0.75))

  # print for a preview
  print(rlin)

  # evaluate
  rlin(c(0, 0.5, 1))

  # Luminance rescaler
  pal_luv_blues <- pal_luv_bezier(mat_luv_blues)
  rlum <- rescaler_lum(c(40, 70), pal_luv_blues)

  # print for a preview
  print(rlum)

  # evaluate
  rlum(c(0, 0.5, 1))

}
