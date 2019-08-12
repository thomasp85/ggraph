#' Draw the rectangles in a treemap
#'
#' A treemap is a space filling layout that recursively divides a rectangle to
#' the children of the node. Often only the leaf nodes are drawn as nodes higher
#' up in the hierarchy would obscure what is below. `geom_treemap` is a
#' shorthand for `geom_node_treemap` as node is implicit in the case of
#' treemap drawing
#'
#' @section Aesthetics:
#' `geom_treemap` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **width**
#' - **height**
#' - alpha
#' - colour
#' - fill
#' - size
#' - stroke
#' - filter
#'
#' @inheritParams ggplot2::geom_tile
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x, y, width and height are mapped to
#' x, y, width and height in the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' # Create a graph of the flare class system
#' library(tidygraph)
#' flareGraph <- tbl_graph(flare$vertices, flare$edges) %>%
#'   mutate(
#'     class = map_bfs_chr(node_is_root(), .f = function(node, dist, path, ...) {
#'       if (dist <= 1) {
#'         return(shortName[node])
#'       }
#'       path$result[[nrow(path)]]
#'     })
#'   )
#'
#' ggraph(flareGraph, 'treemap', weight = size) +
#'   geom_node_tile(aes(fill = class, filter = leaf, alpha = depth), colour = NA) +
#'   geom_node_tile(aes(size = depth), colour = 'white') +
#'   scale_alpha(range = c(1, 0.5), guide = 'none') +
#'   scale_size(range = c(4, 0.2), guide = 'none')
#' @export
#'
geom_node_tile <- function(mapping = NULL, data = NULL, position = 'identity',
                           show.legend = NA, ...) {
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, width = width, height = height
  ))
  layer(
    data = data, mapping = mapping, stat = StatFilter, geom = GeomNodeTile,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = list(na.rm = FALSE, ...)
  )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
#'
GeomNodeTile <- ggproto('GeomNodeTile', GeomTile,
  default_aes = aes(
    fill = NA, colour = 'black', size = 0.5, linetype = 1,
    alpha = NA, width = 1, height = 1
  ),
  required_aes = c('x', 'y')
)
