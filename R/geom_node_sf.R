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
#'
#' @inheritParams ggplot2::geom_sf
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x and y are mapped to x and y in
#' the node data.
#'
#' @author Lorena Abad
#'
#' @family geom_node_*
#'
#' @examples
#' if (require("sfnetworks", quietly = TRUE)) {
#'   gr <- as_sfnetwork(roxel) %>%
#'     mutate(centrality = centrality_betweenness())
#'   ggraph(gr, 'sf') + geom_node_sf(aes(color = centrality))
#' }
#'
#' @export
#'
geom_node_sf <- function(mapping = NULL, data = get_sf_nodes(), stat = 'sf',
                          position = 'identity', show.legend = NA, ...) {
  c(
    layer_sf(
      geom = GeomSf, data = data, mapping = mapping, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = FALSE,
      params = list(na.rm = FALSE, ...)
    ),
    coord_sf(default = TRUE)
  )
}

get_sf_nodes <- function(){
  function(layout) {
    nodes <- sf::st_as_sf(attr(layout, "graph"), "nodes")
    attr(nodes, 'type_ggraph') <- 'node_ggraph'
    nodes
  }
}
