#' Show nodes as POINTs in geographical space
#'
#' This geom is equivalent in functionality to [ggplot2::geom_sf()] for `POINT`
#' geometries and allows for plotting of nodes in their geographical space in
#' different shapes, colours and sizes.
#'
#' @section Aesthetics:
#' `geom_node_sf` understand the following aesthetics.
#'
#' - alpha
#' - colour
#' - shape
#' - size
#' - filter
#'
#' @inheritParams ggplot2::geom_sf
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default geometry is mapped to the geometry in
#' the node data.
#'
#' @author Lorena Abad
#'
#' @family geom_node_*
#'
#' @examples
#' library(tidygraph)
#'
#' if (require("sfnetworks", quietly = TRUE)) {
#'   gr <- sfnetworks::as_sfnetwork(roxel)
#'   ggraph(gr, 'sf') +
#'     geom_node_sf(aes(color = centrality_betweenness()))
#' }
#'
#' @export
#'
geom_node_sf <- function(
  mapping = NULL,
  data = get_sf_nodes(),
  position = 'identity',
  show.legend = NA,
  ...
) {
  mapping <- aes_intersect(mapping, aes(geometry = geometry))
  c(
    layer_sf(
      geom = GeomSf,
      data = data,
      mapping = mapping,
      stat = StatFilterSf,
      position = position,
      show.legend = show.legend,
      inherit.aes = FALSE,
      params = list2(na.rm = FALSE, ...)
    ),
    coord_sf(default = TRUE)
  )
}

#' @rdname get_nodes
get_sf_nodes <- function() {
  function(layout) {
    nodes <- sf::st_as_sf(layout)
    attr(nodes, 'type_ggraph') <- 'node_ggraph'
    nodes
  }
}
