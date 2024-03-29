% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edge_colourbar.R
\name{guide_edge_colourbar}
\alias{guide_edge_colourbar}
\alias{guide_edge_colorbar}
\title{Colourbar legend for edges}
\usage{
guide_edge_colourbar(..., available_aes = c("edge_colour", "edge_fill"))

guide_edge_colorbar(..., available_aes = c("edge_colour", "edge_fill"))
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[ggplot2:guide_colourbar]{ggplot2::guide_colourbar}}
  \describe{
    \item{\code{title}}{A character string or expression indicating a title of guide.
If \code{NULL}, the title is not shown. By default
(\code{\link[ggplot2:waiver]{waiver()}}), the name of the scale object or the name
specified in \code{\link[ggplot2:labs]{labs()}} is used for the title.}
    \item{\code{theme}}{A \code{\link[ggplot2:theme]{theme}} object to style the guide individually or
differently from the plot's theme settings. The \code{theme} argument in the
guide overrides, and is combined with, the plot's theme.}
    \item{\code{nbin}}{A numeric specifying the number of bins for drawing the
colourbar. A smoother colourbar results from a larger value.}
    \item{\code{display}}{A string indicating a method to display the colourbar. Can be
one of the following:
\itemize{
\item \code{"raster"} to display as a bitmap image.
\item \code{"rectangles"} to display as a series of rectangles.
\item \code{"gradient"} to display as a linear gradient.
}

Note that not all devices are able to render rasters and gradients.}
    \item{\code{raster}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}  A logical. If \code{TRUE} then
the colourbar is rendered as a raster object. If \code{FALSE} then the colourbar
is rendered as a set of rectangles. Note that not all graphics devices are
capable of rendering raster image.}
    \item{\code{alpha}}{A numeric between 0 and 1 setting the colour transparency of
the bar. Use \code{NA} to preserve the alpha encoded in the colour itself
(default).}
    \item{\code{draw.ulim}}{A logical specifying if the upper limit tick marks should
be visible.}
    \item{\code{draw.llim}}{A logical specifying if the lower limit tick marks should
be visible.}
    \item{\code{position}}{A character string indicating where the legend should be
placed relative to the plot panels.}
    \item{\code{direction}}{A character string indicating the direction of the guide.
One of "horizontal" or "vertical."}
    \item{\code{reverse}}{logical. If \code{TRUE} the colourbar is reversed. By default,
the highest value is on the top and the lowest value is on the bottom}
    \item{\code{order}}{positive integer less than 99 that specifies the order of
this guide among multiple guides. This controls the order in which
multiple guides are displayed, not the contents of the guide itself.
If 0 (default), the order is determined by a secret algorithm.}
  }}

\item{available_aes}{A vector of character strings listing the aesthetics
for which a colourbar can be drawn.}
}
\value{
A guide object
}
\description{
This function is equivalent to \code{\link[ggplot2:guide_colourbar]{ggplot2::guide_colourbar()}} but
works for edge aesthetics.
}
