#' Draw edges as diagonals
#'
#' This geom draws edge loops (edges starting and ending at the same node).
#' Loops are drawn as bezier curves starting and ending at the position of the
#' node and with control points protruding at an angle and in a direction
#' specified in the call. As the start and end node is always the same no *2
#' method is provided. Loops can severely clutter up your visualization which is
#' why they are decoupled from the other edge drawings. Only plot them if they
#' are of importance. If the graph doesn't contain any loops the geom adds
#' nothing silently.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_loop` and `geom_edge_loop0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **from**
#' - **to**
#' - **span** *90*
#' - **direction** *45*
#' - **strength** *1*
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_loop` furthermore takes the following aesthetics.
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
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- as_tbl_graph(
#'   data.frame(from = c(1, 1, 2, 2, 3, 3, 3), to = c(1, 2, 2, 3, 3, 1, 2))
#' )
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_loop(aes(alpha = stat(index))) +
#'   geom_edge_fan(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_loop0() +
#'   geom_edge_fan0()
#' @rdname geom_edge_loop
#' @name geom_edge_loop
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeLoop <- ggproto('StatEdgeLoop', StatBezier,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data <- data[data$from == data$to, ]
    data$group <- make_unique(data$group)
    if (nrow(data) != 0) {
      create_loops(data, params)
    } else {
      NULL
    }
  },
  required_aes = c('x', 'y', 'from', 'to', 'span', 'direction', 'strength'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n')
)
#' @rdname geom_edge_loop
#'
#' @export
geom_edge_loop <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL, n = 100,
                           lineend = 'butt', linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, from = from, to = to, group = edge.id,
    span = 90, direction = 45, strength = 1
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLoop,
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
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeLoop0 <- ggproto('StatEdgeLoop0', StatBezier0,
  setup_data = function(data, params) {
    StatEdgeLoop$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'from', 'to', 'span', 'direction', 'strength'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm')
)
#' @rdname geom_edge_loop
#'
#' @export
geom_edge_loop0 <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', arrow = NULL,
                            lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, from = from, to = to,
    span = 90, direction = 45, strength = 1
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeLoop0,
    geom = GeomEdgeBezier, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(arrow = arrow, lineend = lineend, na.rm = FALSE, ...)
    )
  )
}

create_loops <- function(loops, params) {
  control_angle1 <- loops$direction - loops$span / 2
  control_angle2 <- loops$direction + loops$span / 2
  controls1 <- find_loop_controls(loops, control_angle1)
  controls2 <- find_loop_controls(loops, control_angle2)
  end <- loops
  bezier_start <- seq(1, by = 4, length.out = nrow(loops))
  loops$index <- bezier_start
  controls1$index <- bezier_start + 1
  controls2$index <- bezier_start + 2
  end$index <- bezier_start + 3
  loops <- rbind_dfs(list(loops, controls1, controls2, end))
  loops[order(loops$index), names(loops) != 'index']
}
find_loop_controls <- function(loops, angle) {
  angle <- angle / 360 * 2 * pi
  loops$x <- loops$x + cos(angle) * loops$strength
  loops$y <- loops$y + sin(angle) * loops$strength
  loops
}
