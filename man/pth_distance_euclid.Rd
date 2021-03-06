% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance-new.R
\name{pth_distance_euclid}
\alias{pth_distance_euclid}
\alias{pth_distance_metric}
\title{Color distance calculations}
\usage{
pth_distance_euclid(
  color_a,
  color_b = NULL,
  transformer = NULL,
  non_luminance_weight = 1,
  ...
)

pth_distance_metric(
  color_a,
  color_b = NULL,
  method = c("cie2000", "cie94", "cie1976", "cmc")
)
}
\arguments{
\item{color_a, color_b}{Objects that can be coerced into colors,
i.e. \code{pth_hex} or \code{pth_mat}.}

\item{transformer}{\code{function} used to transform the colors to new
color space, e.g. \link{pth_to_cieluv}.}

\item{non_luminance_weight}{\code{numeric} used to "discount" the effects of
chroma and hue in the distance calculation.}

\item{...}{additional arguments passed on to \code{transformer}.}

\item{method}{\code{character} metric used by \code{farver::compare_colour()}.}
}
\value{
\code{double} one value representing distance for each color comparison.
}
\description{
There are two ways to make a color-distance calculation:
\code{pth_distance_euclid()}, which calculates a Euclidean distance within
a color space, or \code{pth_distance_metric()}, which uses a color metric
in \code{\link[farver:compare_colour]{farver::compare_colour()}} to calculate the distance. Note that
distances calculated using \code{pth_distance_metric()} may not follow the
triangle inequality.
}
\details{
For both of these functions, you can specify two sets of colors:
\code{color_a} and \code{color_b}. Each of these has to be a vector of hex codes
(character or \code{pth_hex}), or a matrix-based collection of colors (\code{pth_mat})
using a specific color space, e.g. \code{cieluv}.

For both functions, the number of colors in each of \code{color_a} and \code{color_b}
determines the distance measurements that are returned:
\itemize{
\item If \code{color_b} is \code{NULL}, it returns the distances between consecutive colors
in \code{color_a}. The length of the returned vector will be one less than the
number of colors in \code{color_a}.
\item If \code{color_b} has only one color, it returns the distances between each
color in \code{color_a} and the one color in \code{color_b}. The length of the returned
vector will be equal to the number of colors in \code{color_a}.
\item If \code{color_a} and \code{color_b} have equal numbers of colors, it returns the
pairwise distances between each color in \code{color_a} and \code{color_b}. The length
of the returned vector will be equal to the number of colors in \code{color_a}
(and \code{color_b}).
\item For any other arrangement, an error will be thrown.
}

For \code{pth_distance_euclid()}, you can specify the color space in which you
wish to make the distance calculation by specifying a function that converts
to that color space. For example, if you want to use the Jzazbz-100 color
space, set \code{transformer} to \code{pth_to_jzazbz100}. You can use the \code{...} to
provide additional arguments to the \code{transformer} function.

The default behavior of \code{transformer} depends on \code{color_a} and \code{color_b}:
\itemize{
\item If \code{color_a} and \code{color_b} are both hex codes, default is to use
\link{pth_to_cieluv}.
\item If \code{color_a} and \code{color_b} are both specified using the same color space,
the default is to use the \emph{that} color space.
\item If \code{color_a} and \code{color_b} use difference color spaces or representations,
the default is not determined; you have to specify a \code{transformer} or an
error is thrown.
}

At present, the argument \code{non_luminance_weight} is an experiment. This gives
you a way to weight the perceptual distances to favor luminance. For example,
if you set \code{non_luminance_weight} to zero, it will return the differences in
luminance in the given color space.

The \code{pth_distance_metric()} uses \code{\link[farver:compare_colour]{farver::compare_colour()}} to calculate
the distance between colors. These methods use metric functions; the triangle
inequality is not necessarily satisfied. You can specify the \code{method}; this is
passed on to \code{\link[farver:compare_colour]{farver::compare_colour()}}.
}
\examples{
hex <- c("#000000", "#663399", "#ffffff")
luv <- pth_to_cieluv(hex)

# calculates distances between consecutive elements of `hex`,
# when providing `pth_hex`, the default is to use `cieluv` color space:
pth_distance_euclid(hex)

# if we provide a matrix that has a color space, the distances are
# calculated using that color space; in this case we expect the same
# distances as above:
pth_distance_euclid(luv)

# we can calculate both using a different color space by specifying a
# `transformer`; we will get a different set of distances from above,
# but they remain internally consistent:
pth_distance_euclid(hex, transformer = pth_to_cam02ucs)
pth_distance_euclid(luv, transformer = pth_to_cam02ucs)

# to calculate pairwise distances, specify `color_a` and `color_b`,
# each with the same length:
pth_distance_euclid(hex[1:2], hex[2:3])

# to calculate distances from a reference, set `color_b` to the reference:
pth_distance_euclid(hex, hex[1])

# using a metric follows the same principles; the calculation uses
# farver::compare_colour(), you can specify the `method`:
pth_distance_metric(hex, method = "cie94")

# calculate distance for adjacent colors:
pth_distance_metric(hex)

# calculate pairwise distances:
pth_distance_metric(hex[1:2], hex[2:3])

# calculate distance from reference:
pth_distance_metric(hex, hex[1])

}
