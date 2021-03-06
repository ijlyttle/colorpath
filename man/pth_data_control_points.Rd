% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-data.R
\name{pth_data_control_points}
\alias{pth_data_control_points}
\alias{pth_data_control_points.default}
\alias{pth_data_control_points.pth_chroma_trajectory}
\alias{pth_data_control_points.pth_palette_path}
\title{Dataset for trajectories}
\usage{
pth_data_control_points(x, ...)

\method{pth_data_control_points}{default}(x, ...)

\method{pth_data_control_points}{pth_chroma_trajectory}(x, ...)

\method{pth_data_control_points}{pth_palette_path}(x, ...)
}
\arguments{
\item{x}{\code{function} with S3 class \code{pth_chroma_trajectory} or
\code{pth_palette_path}.}

\item{...}{other arguments (not used).}
}
\value{
\code{tibble} with columns \code{luminance}, \code{chroma}
}
\description{
Dataset for trajectories
}
