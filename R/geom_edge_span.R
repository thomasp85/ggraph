#' Draw edges as vertical spans
#'
#' This edge geom is mainly intended for use with [fabric][layout_tbl_graph_fabric]
#' layouts. It draws edges as vertical segments with an optional end shape
#' adornment. Due to the special nature of fabric layouts where nodes are not
#' a single point in space but a line, this geom doesn't derive the x position
#' from the location of the terminal nodes, but defaults to using the `edge_x`
#' variable calculated by the fabric layout. If this geom is used with other
#' layouts `x`and `xend` must be given explicitly.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_span` and `geom_edge_span0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_span2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_span` and `geom_edge_span2` furthermore takes the following
#' aesthetics.
#'
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param end_shape The adornment to put at the ends of the span. The naming
#' follows the conventions of the shape aesthetic in [ggplot2::geom_point()]
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- play_smallworld(n_dim = 3, dim_size = 3, order = 1, p_rewire = 0.6)
#'
#' # Standard use
#' ggraph(gr, 'fabric', sort.by = node_rank_fabric()) +
#'   geom_node_range(colour = 'grey80') +
#'   geom_edge_span()
#'
#' # Add end shapes
#' ggraph(gr, 'fabric', sort.by = node_rank_fabric()) +
#'   geom_node_range(colour = 'grey80') +
#'   geom_edge_span(end_shape = 'circle')
#'
#' # If the layout include shadow edges these can be styled differently
#' ggraph(gr, 'fabric', sort.by = node_rank_fabric(), shadow.edges = TRUE) +
#'   geom_node_range(colour = 'grey80') +
#'   geom_edge_span(aes(colour = shadow_edge), end_shape = 'square') +
#'   scale_edge_colour_manual(values = c('FALSE' = 'black', 'TRUE' = 'grey'))
#'
#' @rdname geom_edge_span
#' @name geom_edge_span
#'
NULL

#' @rdname geom_edge_span
#'
#' @importFrom ggforce StatLink
#' @export
geom_edge_span <- function(mapping = NULL, data = get_edges('short'),
                           position = 'identity', end_shape = NA, arrow = NULL, n = 100,
                           lineend = 'butt', linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = edge_x, y = y, xend = edge_x,
                                        yend = yend, group = edge.id))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLink,
    geom = GeomEdgeSpanPath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, end_shape = end_shape,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname geom_edge_span
#'
#' @importFrom ggforce StatLink2
#' @export
geom_edge_span2 <- function(mapping = NULL, data = get_edges('long'),
                            position = 'identity', end_shape = NA, arrow = NULL, n = 100,
                            lineend = 'butt', linejoin = 'round', linemitre = 1,
                            label_colour = 'black', label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = edge_x, y = y,
                                        group = edge.id))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLink2,
    geom = GeomEdgeSpanPath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, end_shape = end_shape,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname geom_edge_span
#'
#' @importFrom ggforce StatLink2
#' @export
geom_edge_span0 <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', end_shape = NA, arrow = NULL,
                            lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = edge_x, y = y,
                                        xend = edge_x, yend = yend))
  layer(
    data = data, mapping = mapping, stat = StatFilter,
    geom = GeomEdgeSpanSegment, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list(end_shape = end_shape, arrow = arrow, lineend = lineend, na.rm = FALSE, ...)
    )
  )
}
