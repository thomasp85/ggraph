#' Show nodes as voronoi tiles
#'
#' This geom is equivalent in functionality to [ggforce::geom_voronoi_tile()]
#' and allows for plotting of nodes as tiles from a voronoi tesselation. As with
#' [ggforce::geom_voronoi_tile()] it is possible to restrict the size of the
#' tile to a fixed radius, as well as round corners and expand/contract the
#' tile.
#'
#' @section Aesthetics:
#' `geom_node_voronoi` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - alpha
#' - colour
#' - fill
#' - shape
#' - size
#' - stroke
#' - filter
#'
#' @inheritParams ggforce::geom_voronoi_tile
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x and y are mapped to x and y in
#' the node data and group set to `-1`.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('meredith') %>%
#'   mutate(group = sample(letters[1:4], n(), TRUE))
#'
#' ggraph(gr) +
#'   geom_node_voronoi(aes(fill = group, colour = group), alpha = 0.3) +
#'   geom_edge_link(alpha = 0.3) +
#'   geom_node_point()
#'
#' # Use max.radius to make the tesselation more "node"-like
#' ggraph(gr) +
#'   geom_node_voronoi(aes(fill = group, colour = group), alpha = 0.3, max.radius = 1) +
#'   geom_edge_link(alpha = 0.3) +
#'   geom_node_point()
#' @export
#' @importFrom ggforce GeomShape
#'
geom_node_voronoi <- function(mapping = NULL, data = NULL, position = 'identity',
                              show.legend = NA, bound = NULL, eps = 1e-09,
                              max.radius = NULL, normalize = FALSE,
                              asp.ratio = 1, expand = 0, radius = 0, ...) {
  mapping <- aes_intersect(mapping, aes(x = x, y = y, group = -1))
  layer(
    data = data, mapping = mapping, stat = StatNodeVoronoi, geom = GeomShape,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = list(na.rm = FALSE, bound = bound, eps = eps, max.radius = max.radius,
                  normalize = normalize, asp.ratio = asp.ratio, expand = expand,
                  radius = radius, ...)
  )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatVoronoiTile
#' @export
StatNodeVoronoi <- ggproto('StatNodeVoronoi', StatVoronoiTile,
  finish_layer = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data
  },
  default_aes = aes(filter = TRUE)
)
