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
#'
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
        data <- drop_loop(data, warn=params$warn_hidden_loop)
        data$group <- seq_len(nrow(data))
        data2 <- data
        data2$x <- data2$xend
        data2$y <- data2$yend
        data$xend <- NULL
        data$yend <- NULL
        data2$xend <- NULL
        data2$yend <- NULL
        keep <- atan2(data$y, data$x) != atan2(data2$y, data2$x)
        createHiveBezier(data[keep, ], data2[keep, ], params)
    },
    required_aes = c('x', 'y', 'xend', 'yend'),
    default_aes = aes(filter = TRUE),
    extra_params = c(StatBezier$extra_params, 'curvature', 'warn_hidden_loop')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive <- function(mapping = NULL, data = get_edges(),
                           position = "identity", arrow = NULL,
                           curvature = 0.5, n = 100, lineend = "butt",
                           linejoin = "round", linemitre = 1,
                           label_colour = 'black',  label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, warn_hidden_loop = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend))
    layer(data = data, mapping = mapping, stat = StatEdgeHive,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = FALSE, curvature = curvature,
                   label_colour = label_colour, label_alpha = label_alpha,
                   label_parse = label_parse, check_overlap = check_overlap,
                   angle_calc = angle_calc, force_flip = force_flip,
                   label_dodge = label_dodge, label_push = label_push, ...)
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
        data <- drop_loop(data, warn=params$warn_hidden_loop)
        data <- data[order(data$group),]
        data2 <- data[c(FALSE, TRUE), ]
        data <- data[c(TRUE, FALSE), ]
        keep <- atan2(data$y, data$x) != atan2(data2$y, data2$x)
        createHiveBezier(data[keep, ], data2[keep, ], params)
    },
    required_aes = c('x', 'y', 'group'),
    default_aes = aes(filter = TRUE),
    extra_params = c(StatBezier2$extra_params, 'curvature', 'warn_hidden_loop')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive2 <- function(mapping = NULL, data = get_edges('long'),
                            position = "identity", arrow = NULL,
                            curvature = 0.5, n = 100, lineend = "butt",
                            linejoin = "round", linemitre = 1,
                            label_colour = 'black',  label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, warn_hidden_loop = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge.id))
    layer(data = data, mapping = mapping, stat = StatEdgeHive2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = TRUE, curvature = curvature,
                   label_colour = label_colour, label_alpha = label_alpha,
                   label_parse = label_parse, check_overlap = check_overlap,
                   angle_calc = angle_calc, force_flip = force_flip,
                   label_dodge = label_dodge, label_push = label_push, ...)
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
        data <- drop_loop(data, warn=params$warn_hidden_loop)
        StatEdgeHive$setup_data(data, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend'),
    default_aes = aes(filter = TRUE),
    extra_params = c(StatBezier$extra_params, 'curvature', 'warn_hidden_loop')
)
#' @rdname geom_edge_hive
#'
#' @export
geom_edge_hive0 <- function(mapping = NULL, data = get_edges(),
                           position = "identity", arrow = NULL, curvature = 0.5,
                           lineend = "butt", show.legend = NA,
                           warn_hidden_loop = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend))
    layer(data = data, mapping = mapping, stat = StatEdgeHive0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                        curvature = curvature, ...)
          )
    )
}
createHiveBezier <- function(from, to, params) {
    bezierStart <- seq(1, by=4, length.out = nrow(from))
    from$index <- bezierStart
    to$index <- bezierStart + 3
    data2 <- from
    data3 <- to
    data2$index <- bezierStart + 1
    data3$index <- bezierStart + 2
    fromAxis <- atan2(from$y, from$x)
    fromAxis[fromAxis < 0] <- fromAxis[fromAxis < 0] + 2 * pi
    toAxis <- atan2(to$y, to$x)
    toAxis[toAxis < 0] <- toAxis[toAxis < 0] + 2 * pi
    fromFirst <- ifelse(fromAxis < toAxis,
                        toAxis - fromAxis < pi,
                        toAxis - fromAxis < -pi)
    middleAxis1 <- ifelse(fromFirst, fromAxis, toAxis)
    middleAxis2 <- ifelse(fromFirst, toAxis, fromAxis)
    middleAxis2 <- ifelse(middleAxis2 < middleAxis1, middleAxis2 + 2*pi, middleAxis2)
    meanAxis <- (middleAxis2 - middleAxis1)/2
    middleAxis1 <- middleAxis1 + meanAxis * params$curvature
    middleAxis2 <- middleAxis2 - meanAxis * params$curvature
    nodeR2 <- sqrt(data2$x^2 + data2$y^2)
    nodeR3 <- sqrt(data3$x^2 + data3$y^2)
    data2$x <- nodeR2 * cos(ifelse(fromFirst, middleAxis1, middleAxis2))
    data2$y <- nodeR2 * sin(ifelse(fromFirst, middleAxis1, middleAxis2))
    data3$x <- nodeR3 * cos(ifelse(fromFirst, middleAxis2, middleAxis1))
    data3$y <- nodeR3 * sin(ifelse(fromFirst, middleAxis2, middleAxis1))
    data <- rbind(from, data2, data3, to)
    data[order(data$index), names(data) != 'index']
}
