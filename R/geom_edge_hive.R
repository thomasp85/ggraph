#' Draw edges in hive plots
#'
#' This geom is only intended for use together with the hive layout. It draws
#' edges between nodes as bezier curves, with the control points positioned at
#' the same radii as the start or end point, and at a distance defined by the
#' curvature argument.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_hive` and `geom_edge_hive0` understand the following
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
#' `geom_edge_hive2` understand the following aesthetics. Bold aesthetics are
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
#' `geom_edge_hive` and `geom_edge_hive2` furthermore takes the following
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
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param curvature The curvature of the bezier. Defines the distance from the
#' control points to the midpoint between the start and end node. 1 means the
#' control points are positioned midway between the nodes, while 0 means it
#' coincide with the nodes (resulting in straight lines)
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' # Plot the flare import graph as a hive plot
#' library(tidygraph)
#' flareGr <- as_tbl_graph(flare$imports) %>%
#'   mutate(
#'     type = dplyr::case_when(
#'       centrality_degree(mode = 'in') == 0 ~ 'Source',
#'       centrality_degree(mode = 'out') == 0 ~ 'Sink',
#'       TRUE ~ 'Both'
#'     )
#'   ) %>%
#'   activate(edges) %>%
#'   mutate(
#'     type = dplyr::case_when(
#'       grepl('flare.analytics', paste(.N()$name[from], .N()$name[to])) ~ 'Analytics',
#'       TRUE ~ 'Other'
#'     )
#'   )
#'
#' ggraph(flareGr, 'hive', axis = type) +
#'   geom_edge_hive(aes(colour = type), edge_alpha = 0.1) +
#'   coord_fixed()
#' @rdname geom_edge_hive
#' @name geom_edge_hive
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeHive <- ggproto('StatEdgeHive', StatBezier,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data$group <- seq_len(nrow(data))
    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    data$xend <- NULL
    data$yend <- NULL
    data2$xend <- NULL
    data2$yend <- NULL
    keep <- atan2(data$y, data$x) != atan2(data2$y, data2$x)
    create_hive_bezier(data[keep, ], data2[keep, ], params)
  },
  required_aes = c('x', 'y', 'xend', 'yend'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'curvature')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL,
                           curvature = 0.5, n = 100, lineend = 'butt',
                           linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        xend = xend, yend = yend))
  layer(
    data = data, mapping = mapping, stat = StatEdgeHive,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, curvature = curvature,
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
StatEdgeHive2 <- ggproto('StatEdgeHive2', StatBezier2,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data <- data[order(data$group), ]
    data2 <- data[c(FALSE, TRUE), ]
    data <- data[c(TRUE, FALSE), ]
    keep <- atan2(data$y, data$x) != atan2(data2$y, data2$x)
    create_hive_bazier(data[keep, ], data2[keep, ], params)
  },
  required_aes = c('x', 'y', 'group'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'curvature')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive2 <- function(mapping = NULL, data = get_edges('long'),
                            position = 'identity', arrow = NULL,
                            curvature = 0.5, n = 100, lineend = 'butt',
                            linejoin = 'round', linemitre = 1,
                            label_colour = 'black', label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        group = edge.id))
  layer(
    data = data, mapping = mapping, stat = StatEdgeHive2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, curvature = curvature,
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
StatEdgeHive0 <- ggproto('StatEdgeHive0', StatBezier0,
  setup_data = function(data, params) {
    StatEdgeHive$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'curvature')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive0 <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', arrow = NULL, curvature = 0.5,
                            lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        xend = xend, yend = yend))
  layer(
    data = data, mapping = mapping, stat = StatEdgeHive0,
    geom = GeomEdgeBezier, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        curvature = curvature, ...
      )
    )
  )
}
create_hive_bezier <- function(from, to, params) {
  bezier_start <- seq(1, by = 4, length.out = nrow(from))
  from$index <- bezier_start
  to$index <- bezier_start + 3
  data2 <- from
  data3 <- to
  data2$index <- bezier_start + 1
  data3$index <- bezier_start + 2
  from_axis <- atan2(from$y, from$x)
  from_axis[from_axis < 0] <- from_axis[from_axis < 0] + 2 * pi
  to_axis <- atan2(to$y, to$x)
  to_axis[to_axis < 0] <- to_axis[to_axis < 0] + 2 * pi
  from_first <- ifelse(from_axis < to_axis,
    to_axis - from_axis < pi,
    to_axis - from_axis < -pi
  )
  middle_axis1 <- ifelse(from_first, from_axis, to_axis)
  middle_axis2 <- ifelse(from_first, to_axis, from_axis)
  middle_axis2 <- ifelse(middle_axis2 < middle_axis1, middle_axis2 + 2 * pi, middle_axis2)
  mean_axis <- (middle_axis2 - middle_axis1) / 2
  middle_axis1 <- middle_axis1 + mean_axis * params$curvature
  middle_axis2 <- middle_axis2 - mean_axis * params$curvature
  node_r2 <- sqrt(data2$x^2 + data2$y^2)
  node_r3 <- sqrt(data3$x^2 + data3$y^2)
  data2$x <- node_r2 * cos(ifelse(from_first, middle_axis1, middle_axis2))
  data2$y <- node_r2 * sin(ifelse(from_first, middle_axis1, middle_axis2))
  data3$x <- node_r3 * cos(ifelse(from_first, middle_axis2, middle_axis1))
  data3$y <- node_r3 * sin(ifelse(from_first, middle_axis2, middle_axis1))
  data <- rbind(from, data2, data3, to)
  data[order(data$index), names(data) != 'index']
}
