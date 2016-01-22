#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatLink
#' @export
StatEdgeLink <- ggproto('StatEdgeLink', StatLink,
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        StatLink$setup_data(data, params)
    }
)
#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatLink2
#' @export
StatEdgeLink2 <- ggproto('StatEdgeLink2', StatLink2,
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        StatLink2$setup_data(data, params)
    }
)
#' @importFrom ggplot2 layer aes
#' @importFrom ggforce StatLink
#' @export
geom_edge_link <- function(mapping = NULL, data = gEdges('short'), stat = "edge_link",
                           position = "identity", arrow = NULL,
                           lineend = "butt", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, n=100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = FALSE, ...)
    )
}
#' @importFrom ggplot2 layer aes
#' @importFrom ggforce StatLink2
#' @export
geom_edge_link2 <- function(mapping = NULL, data = gEdges('long'), stat = "edge_link2",
                           position = "identity", arrow = NULL,
                           lineend = "butt", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, n=100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, group=edge.id))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = TRUE, ...)
    )
}
#' @importFrom ggplot2 layer aes
#' @importFrom ggforce StatLink2
#' @export
geom_edge_link0 <- function(mapping = NULL, data = gEdges(), stat = "filter",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeSegment,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
