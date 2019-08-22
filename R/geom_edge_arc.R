#' Draw edges as Arcs
#'
#' This geom is mainly intended for arc linear and circular diagrams (i.e. used
#' together with [layout_tbl_graph_linear()]), though it can be used
#' elsewhere. It draws edges as arcs with a height proportional to the distance
#' between the nodes. Arcs are calculated as beziers. For linear layout the
#' placement of control points are related to the `curvature` argument and
#' the distance between the two nodes. For circular layout the control points
#' are placed on the same angle as the start and end node at a distance related
#' to the distance between the nodes.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_arc` and `geom_edge_arc0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **circular**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_arc2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **circular**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_arc` and `geom_edge_arc2` furthermore takes the following
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
#' @param strength The bend of the curve. 1 approximates a halfcircle while 0
#' will give a straight line. Negative number will change the direction of the
#' curve. Only used if `circular = FALSE`.
#'
#' @param fold Logical. Should arcs appear on the same side of the nodes despite
#' different directions. Default to `FALSE`.
#'
#' @param curvature Deprecated. Use `strength` instead.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' # Make a graph with different directions of edges
#' gr <- create_notable('Meredith') %>%
#'   convert(to_directed) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(
#'     class = sample(letters[1:3], n(), replace = TRUE),
#'     switch = sample(c(TRUE, FALSE), n(), replace = TRUE)
#'   ) %>%
#'   reroute(from = to, to = from, subset = switch)
#'
#' ggraph(gr, 'linear') +
#'   geom_edge_arc(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'linear') +
#'   geom_edge_arc2(aes(colour = node.class), strength = 0.6)
#'
#' ggraph(gr, 'linear', circular = TRUE) +
#'   geom_edge_arc0(aes(colour = class))
#' @rdname geom_edge_arc
#' @name geom_edge_arc
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeArc <- ggproto('StatEdgeArc', StatBezier,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    data$group <- make_unique(data$group)
    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    create_arc(data, data2, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'strength', 'fold', 'curvature')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc <- function(mapping = NULL, data = get_edges(),
                          position = 'identity', arrow = NULL, strength = 1,
                          n = 100, fold = FALSE, lineend = 'butt',
                          linejoin = 'round', linemitre = 1,
                          label_colour = 'black', label_alpha = 1,
                          label_parse = FALSE, check_overlap = FALSE,
                          angle_calc = 'rot', force_flip = TRUE,
                          label_dodge = NULL, label_push = NULL,
                          show.legend = NA, ..., curvature) {
  if (!missing(curvature)) {
    .Deprecated(msg = 'The curvature argument has been deprecated in favour of strength')
    strength <- curvature
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular, group = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeArc,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, strength = strength, fold = fold,
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
#' @importFrom ggforce StatBezier2
#' @export
StatEdgeArc2 <- ggproto('StatEdgeArc2', StatBezier2,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop2(data)
    if (nrow(data) == 0) return(NULL)
    data <- data[order(data$group), ]
    data2 <- data[c(FALSE, TRUE), ]
    data <- data[c(TRUE, FALSE), ]
    create_arc(data, data2, params)
  },
  required_aes = c('x', 'y', 'group', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'strength', 'fold', 'curvature')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc2 <- function(mapping = NULL, data = get_edges('long'),
                           position = 'identity', arrow = NULL, strength = 1,
                           n = 100, fold = FALSE, lineend = 'butt',
                           linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ..., curvature) {
  if (!missing(curvature)) {
    .Deprecated(msg = 'The curvature argument has been deprecated in favour of strength')
    strength <- curvature
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id,
    circular = circular
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeArc2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, strength = strength, fold = fold,
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
StatEdgeArc0 <- ggproto('StatEdgeArc0', StatBezier0,
  setup_data = function(data, params) {
    StatEdgeArc$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'strength', 'fold', 'curvature')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc0 <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL, strength = 1,
                           lineend = 'butt', show.legend = NA, fold = fold, ..., curvature) {
  if (!missing(curvature)) {
    .Deprecated(msg = 'The curvature argument has been deprecated in favour of strength')
    strength <- curvature
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeArc0,
    geom = GeomEdgeBezier, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        strength = strength, fold = FALSE, ...
      )
    )
  )
}

create_arc <- function(from, to, params) {
  bezier_start <- seq(1, by = 4, length.out = nrow(from))
  from$index <- bezier_start
  to$index <- bezier_start + 3
  data2 <- from
  data3 <- to
  data2$index <- bezier_start + 1
  data3$index <- bezier_start + 2
  node_dist <- sqrt((to$x - from$x)^2 + (to$y - from$y)^2) / 2
  circ <- from$circular
  if (any(circ)) {
    r0 <- sqrt(to$x[circ]^2 + to$y[circ]^2)
    r1 <- sqrt(to$x[circ]^2 + to$y[circ]^2)

    data2$x[circ] <- from$x[circ] * (1 - (node_dist[circ] / r0))
    data2$y[circ] <- from$y[circ] * (1 - (node_dist[circ] / r0))
    data3$x[circ] <- to$x[circ] * (1 - (node_dist[circ] / r1))
    data3$y[circ] <- to$y[circ] * (1 - (node_dist[circ] / r1))
  }
  if (any(!circ)) {
    strength <- pi / 2 * -params$strength
    edge_angle <- atan2(
      to$y[!circ] - from$y[!circ],
      to$x[!circ] - from$x[!circ]
    )
    start_angle <- edge_angle - strength
    end_angle <- edge_angle - pi + strength
    data2$x[!circ] <- data2$x[!circ] + cos(start_angle) * node_dist[!circ]
    data2$y[!circ] <- data2$y[!circ] + sin(start_angle) * node_dist[!circ]
    data3$x[!circ] <- data3$x[!circ] + cos(end_angle) * node_dist[!circ]
    data3$y[!circ] <- data3$y[!circ] + sin(end_angle) * node_dist[!circ]
    if (params$fold) {
      # data2$x[!circ] <- abs(data2$x[!circ]) * sign(params$strength)
      data2$y[!circ] <- abs(data2$y[!circ]) * sign(params$strength)
      # data3$x[!circ] <- abs(data3$x[!circ]) * sign(params$strength)
      data3$y[!circ] <- abs(data3$y[!circ]) * sign(params$strength)
    }
  }
  data <- rbind_dfs(list(from, data2, data3, to))
  data[order(data$index), names(data) != 'index']
}
