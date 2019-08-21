#' Show nodes as circles
#'
#' This geom is equivalent in functionality to [ggforce::geom_circle()]
#' and allows for plotting of nodes as circles with a radius scaled by the
#' coordinate system. Because of the geoms reliance on the coordinate system
#' it will only produce true circles when combined with
#' [ggplot2::coord_fixed()]
#'
#' @section Aesthetics:
#' `geom_node_circle` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x0**
#' - **y0**
#' - **r**
#' - alpha
#' - colour
#' - fill
#' - shape
#' - size
#' - stroke
#' - filter
#'
#' @inheritParams ggforce::geom_circle
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x and y are mapped to x0 and y0 in
#' the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(tidygraph)
#' gr <- tbl_graph(flare$vertices, flare$edges)
#' ggraph(gr, 'circlepack', weight = size) +
#'   geom_node_circle() +
#'   coord_fixed()
#' @export
#' @importFrom ggforce GeomCircle
#'
geom_node_circle <- function(mapping = NULL, data = NULL, position = 'identity',
                             show.legend = NA, ...) {
  mapping <- aes_intersect(mapping, aes(x0 = x, y0 = y, r = r))
  layer(
    data = data, mapping = mapping, stat = StatNodeCircle, geom = GeomCircle,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = list(na.rm = FALSE, ...)
  )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatCircle
#' @export
StatNodeCircle <- ggproto('StatNodeCircle', StatCircle,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data$group <- make_unique(data$group)
    data
  },
  default_aes = aes(filter = TRUE)
)
