#' Draw edges as straight lines between nodes
#'
#' This geom draws edges in the simplest way - as straight lines between the
#' start and end nodes. Not much more to say about that...
#'
#' @section Edge variants:
#' Many geom_edge_* layers comes in 3 flavors depending on the level of control
#' needed over the drawing. The default (no numeric postfix) generate a number
#' of points (`n`) along the edge and draws it as a path. Each point along
#' the line has a numeric value associated with it giving the position along the
#' path, and it is therefore possible to show the direction of the edge by
#' mapping to this e.g. `colour = stat(index)`. The version postfixed with a
#' "2" uses the "long" edge format (see [get_edges()]) and makes it
#' possible to interpolate node parameter between the start and end node along
#' the edge. It is considerable less performant so should only be used if this
#' is needed. The version postfixed with a "0" draws the edge in the most
#' performant way, often directly using an appropriate grob from the grid
#' package, but does not allow for gradients along the edge.
#'
#' Often it is beneficial to stop the drawing of the edge before it reaches the
#' node, for instance in cases where an arrow should be drawn and the arrowhead
#' shouldn't lay on top or below the node point. geom_edge_* and geom_edge_*2
#' supports this through the start_cap and end_cap aesthetics that takes a
#' [geometry()] specification and dynamically caps the termini of the
#' edges based on the given specifications. This means that if
#' `end_cap = circle(1, 'cm')` the edges will end at a distance of 1cm even
#' during resizing of the plot window.
#'
#' All `geom_edge_*` and `geom_edge_*2` have the ability to draw a
#' label along the edge. The reason this is not a separate geom is that in order
#' for the label to know the location of the edge it needs to know the edge type
#' etc. Labels are drawn by providing a label aesthetic. The label_pos can be
#' used to specify where along the edge it should be drawn by supplying a number
#' between 0 and 1. The label_size aesthetic can be used to control the size of
#' the label. Often it is needed to have the label written along the direction
#' of the edge, but since the actual angle is dependent on the plot dimensions
#' this cannot be calculated beforehand. Using the angle_calc argument allows
#' you to specify whether to use the supplied angle aesthetic or whether to draw
#' the label along or across the edge.
#'
#' @section Edge aesthetic name expansion:
#' In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' `edge_colour` within the `aes()` call as `colour` will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' `geom_edge_link` and `geom_edge_link0` understand the following
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
#' `geom_edge_link2` understand the following aesthetics. Bold aesthetics are
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
#' `geom_edge_link` and `geom_edge_link2` furthermore takes the following
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
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::geom_text
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x, y, xend, yend, group and
#' circular are mapped to x, y, xend, yend, edge.id and circular in the edge
#' data.
#'
#' @param data The return of a call to `get_edges()` or a data.frame
#' giving edges in correct format (see details for for guidance on the format).
#' See [get_edges()] for more details on edge extraction.
#'
#' @param n The number of points to create along the path.
#'
#' @param label_colour The colour of the edge label. If `NA` it will use
#' the colour of the edge.
#'
#' @param label_alpha The opacity of the edge label. If `NA` it will use
#' the opacity of the edge.
#'
#' @param label_parse If `TRUE`, the labels will be parsed into expressions
#' and displayed as described in [grDevices::plotmath()].
#'
#' @param angle_calc Either 'none', 'along', or 'across'. If 'none' the label will
#' use the angle aesthetic of the geom. If 'along' The label will be written
#' along the edge direction. If 'across' the label will be written across the
#' edge direction.
#'
#' @param force_flip Logical. If `angle_calc` is either 'along' or 'across'
#' should the label be flipped if it is on it's head. Default to `TRUE`.
#'
#' @param label_dodge A [grid::unit()] giving a fixed vertical shift
#' to add to the label in case of `angle_calc` is either 'along' or 'across'
#'
#' @param label_push A [grid::unit()] giving a fixed horizontal shift
#' to add to the label in case of `angle_calc` is either 'along' or 'across'
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_link(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_link2(aes(colour = node.class))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_link0(aes(colour = class))
#' @rdname geom_edge_link
#' @name geom_edge_link
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatLink
#' @export
StatEdgeLink <- ggproto('StatEdgeLink', StatLink,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    StatLink$setup_data(data, params)
  },
  default_aes = aes(filter = TRUE)
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatLink2
#' @export
StatEdgeLink2 <- ggproto('StatEdgeLink2', StatLink2,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop2(data)
    if (nrow(data) == 0) return(NULL)
    StatLink2$setup_data(data, params)
  },
  default_aes = aes(filter = TRUE)
)
#' @rdname geom_edge_link
#'
#' @importFrom ggforce StatLink
#' @export
geom_edge_link <- function(mapping = NULL, data = get_edges('short'),
                           position = 'identity', arrow = NULL, n = 100,
                           lineend = 'butt', linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend, group = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLink,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname geom_edge_link
#'
#' @importFrom ggforce StatLink2
#' @export
geom_edge_link2 <- function(mapping = NULL, data = get_edges('long'),
                            position = 'identity', arrow = NULL, n = 100,
                            lineend = 'butt', linejoin = 'round', linemitre = 1,
                            label_colour = 'black', label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        group = edge.id))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLink2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname geom_edge_link
#'
#' @importFrom ggforce StatLink2
#' @export
geom_edge_link0 <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', arrow = NULL,
                            lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        xend = xend, yend = yend))
  layer(
    data = data, mapping = mapping, stat = StatFilter,
    geom = GeomEdgeSegment, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list(arrow = arrow, lineend = lineend, na.rm = FALSE, ...)
    )
  )
}
