#' Show nodes as circles
#'
#' This geom is equivalent in functionality to \code{\link[ggforce]{geom_circle}}
#' and allows for plotting of nodes as circles with a radius scaled by the
#' coordinate system. Beceause of the geoms reliance on the coordinate system
#' it will only produce true circles when combined with
#' \code{\link[ggplot2]{coord_fixed}}
#'
#' @section Aesthetics:
#' geom_node_point understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{r}}
#'  \item{alpha}
#'  \item{colour}
#'  \item{fill}
#'  \item{shape}
#'  \item{size}
#'  \item{stroke}
#'  \item{filter}
#' }
#'
#' @inheritParams ggforce::geom_circle
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x and y are mapped to x0 and y0 in
#' the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(igraph)
#' gr <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#' ggraph(gr, 'circlepack', weight = 'size') + geom_node_circle() + coord_fixed()
#'
#' @export
#' @importFrom ggforce GeomArcBar
#'
geom_node_arc_bar <- function(mapping = NULL, data = NULL, position = "identity",
                             show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(x0=~0, y0=~0, r0=~r0, r=~r, start=~start, end=~end))
    layer(data = data, mapping = mapping, stat = StatNodeArcBar, geom = GeomArcBar,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, ...)
    )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatArcBar
#' @export
StatNodeArcBar <- ggproto('StatNodeArcBar', StatArcBar,
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        data
    },
    default_aes = aes(filter = TRUE)
)
