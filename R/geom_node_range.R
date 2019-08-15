#' Show nodes as a line spanning a horizontal range
#'
#' This geom is most useful together with the [fabric][layout_tbl_graph_fabric]
#' layout for showing the horizontal span of each node.
#'
#' @section Aesthetics:
#' `geom_node_point` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **xend**
#' - **y**
#' - **yend**
#' - alpha
#' - colour
#' - linetype
#' - size
#' - filter
#'
#' @inheritParams ggplot2::geom_linerange
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x is mapped to xmin, xend is mapped to xmax
#' and y and yend are mapped to y in the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(tidygraph)
#' gr <- as_tbl_graph(highschool)
#'
#' ggraph(gr, layout = 'fabric') +
#'   geom_node_range()
#' @export
#'
geom_node_range <- function(mapping = NULL, data = NULL, position = 'identity',
                            show.legend = NA, ...) {
  mapping <- aes_intersect(mapping, aes(x = xmin, xend = xmax, y = y, yend = y))
  layer(
    data = data, mapping = mapping, stat = StatFilter, geom = GeomSegment,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = list(na.rm = FALSE, ...)
  )
}
