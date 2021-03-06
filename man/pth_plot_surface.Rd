% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-new.R
\name{pth_plot_surface}
\alias{pth_plot_surface}
\alias{pth_plot_surface.default}
\alias{pth_plot_surface.data.frame}
\alias{pth_plot_surface.pth_surface}
\alias{pth_plot_surface.pth_hue_surface}
\alias{pth_plot_surface.pth_palette_path}
\title{Plot hue-surfaces}
\usage{
pth_plot_surface(x, ...)

\method{pth_plot_surface}{default}(x, ...)

\method{pth_plot_surface}{data.frame}(x, ...)

\method{pth_plot_surface}{pth_surface}(x, step = 0.5, ...)

\method{pth_plot_surface}{pth_hue_surface}(x, step = 0.5, constructor = pth_new_cieluv, ...)

\method{pth_plot_surface}{pth_palette_path}(x, step = 0.5, ...)
}
\arguments{
\item{x}{\code{function} with S3 class \code{pth_palette_path} or \code{pth_hue_surface},
or \code{data.frame} with columns \code{luminance}, \code{chroma}, \code{hue}, \code{hex}}

\item{...}{other arguments passed on to \code{constructor}}

\item{step}{\code{numeric} size of step in luminance and chroma.}

\item{constructor}{\code{function} that constructs a \code{pth_mat}, used to
identify the color space for the output matrix.}
}
\value{
\code{tibble} with columns \code{luminance}, \code{chroma}, \code{hue}, \code{hex}
}
\description{
The calculation is a somewhat slow - at the "root" of the speed problem
is the calculation of the maximum chroma. In the future, we could imagine
providing a lookup table, as colorspace does, to speed things up.
}
