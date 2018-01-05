#' Show nodes as thick arcs
#'
#' This geom is equivalent in functionality to \code{\link[ggforce]{geom_arc_bar}}
#' and allows for plotting of nodes as arcs with an inner and outer radius
#' scaled by the coordinate system. Its main use is currently in sunburst plots
#' as created with circular partition layouts
#'
#' @section Aesthetics:
#' geom_node_point understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{r0}}
#'  \item{\strong{r}}
#'  \item{\strong{start}}
#'  \item{\strong{end}}
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
#' require(tidygraph)
#' gr <- tbl_graph(flare$vertices, flare$edges)
#' ggraph(gr, 'partition', circular = TRUE, weight = size) +
#'   geom_node_arc_bar()
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
