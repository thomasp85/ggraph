#' Draw edges as LINESTRINGs in geographical space
#'
#' This geom is equivalent in functionality to [ggplot2::geom_sf()] for `LINESTRING`
#' geometries and allows for plotting of edges in their geographical space in
#' different colours, linetypes and widths.
#'
#' @section Aesthetics:
#' `geom_edge_sf` understand the following aesthetics.
#'
#' - alpha
#' - colour
#' - linetype
#' - filter
#'
#' @inheritParams ggplot2::geom_sf
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default geometry is mapped to the geometry in
#' the edge data.
#'
#' @author Lorena Abad
#'
#' @family geom_edge_*
#'
#' @examples
#' if (require("sfnetworks", quietly = TRUE)) {
#'   gr <- sfnetworks::as_sfnetwork(roxel)
#'   ggraph(gr, 'sf') + geom_edge_sf()
#' }
#'
#' @export
#'
geom_edge_sf <- function(
  mapping = NULL,
  data = get_sf_edges(),
  position = 'identity',
  show.legend = NA,
  ...
) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(geometry = geometry))
  c(
    layer_sf(
      geom = GeomEdgeSf,
      data = data,
      mapping = mapping,
      stat = StatFilterSf,
      position = position,
      show.legend = show.legend,
      inherit.aes = FALSE,
      params = expand_edge_aes(list2(na.rm = FALSE, ...))
    ),
    coord_sf(default = TRUE)
  )
}
#' @rdname get_edges
get_sf_edges <- function() {
  function(layout) {
    edges <- sf::st_as_sf(attr(layout, "graph"), "edges")
    attr(edges, 'type_ggraph') <- 'edge_ggraph'
    edges
  }
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomEdgeSf = ggproto(
  "GeomEdgeSf",
  GeomSf,
  draw_panel = function(data, panel_params, coords) {
    names(data) <- sub('edge_', '', names(data))
    names(data)[names(data) == 'width'] <- 'linewidth'
    GeomSf$draw_panel(data, panel_params, coords)
  },
  draw_key = GeomEdgePath$draw_key,
  default_aes = aes(
    edge_colour = 'black',
    edge_width = 0.5,
    edge_linetype = 1,
    edge_alpha = NA
  ),
  rename_size = FALSE
)
