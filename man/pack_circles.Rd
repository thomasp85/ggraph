% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pack.R
\name{pack_circles}
\alias{pack_circles}
\title{Pack circles together}
\usage{
pack_circles(areas)
}
\arguments{
\item{areas}{A vector of circle areas}
}
\value{
A matrix with two columns and the same number of rows as the length
of the "areas" vector. The matrix has the following attributes added:
"enclosing_radius" giving the radius of the smallest enclosing circle, and
"front_chain" giving the terminating members of the front chain (see
Wang \emph{et al}. 2006).
}
\description{
This function is a direct interface to the circle packing algorithm used by
\code{\link{layout_tbl_graph_circlepack}}. It takes a vector of sizes and
returns the x and y position of each circle as a two-column matrix.
}
\examples{
library(ggforce)
sizes <- sample(10, 100, TRUE)

position <- pack_circles(sizes)
data <- data.frame(x = position[,1], y = position[,2], r = sqrt(sizes/pi))

ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = r), data = data, fill = 'steelblue') +
  geom_circle(aes(x0 = 0, y0 = 0, r = attr(position, 'enclosing_radius'))) +
  geom_polygon(aes(x = x, y = y),
               data = data[attr(position, 'front_chain'), ],
               fill = NA,
               colour = 'black')

}
\references{
Wang, W., Wang, H. H., Dai, G., & Wang, H. (2006). \emph{Visualization of
large hierarchical data by circle packing}. Chi, 517-520.
}
