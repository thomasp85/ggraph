#' Draw edges as LINESTRINGs in geographical space
#' @export
#'
geom_edge_sf <- function(mapping = NULL, data = get_sf_edges(), stat = 'sf',
                         position = 'identity', show.legend = NA, ...) {
  # mapping <- complete_edge_aes(mapping)
  c(
    layer_sf(
      geom = GeomSf, data = data, mapping = mapping, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = FALSE,
      params = list(na.rm = FALSE, ...)
    ),
    coord_sf(default = TRUE)
  )
}

get_sf_edges <- function(){
  function(layout) {
    sf::st_as_sf(attr(layout, "graph"), "edges")
  }
}
