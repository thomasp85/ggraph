#' @importFrom ggplot2 layer aes
#' @importFrom ggforce StatLink
#' @export
geom_edge_link <- function(mapping = NULL, data = gEdges('short'), stat = "link",
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
geom_edge_link2 <- function(mapping = NULL, data = gEdges('long'), stat = "link2",
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
