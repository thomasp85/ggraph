#' Show nodes as points
#'
#' This geom is equivalent in functionality to [ggplot2::geom_point()]
#' and allows for simple plotting of nodes in different shapes, colours and sizes.
#'
#' @section Aesthetics:
#' `geom_node_point` understand the following aesthetics. Bold aesthetics are
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
#' @inheritParams ggplot2::geom_point
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x and y are mapped to x and y in
#' the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'stress') + geom_node_point()
#' @export
#'
geom_node_point <- function(mapping = NULL, data = NULL, position = 'identity',
                            show.legend = NA, ...) {
  mapping <- aes_intersect(mapping, aes(x = x, y = y))
  layer(
    data = data, mapping = mapping, stat = StatFilter, geom = GeomPoint,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = list(na.rm = FALSE, ...)
  )
}
