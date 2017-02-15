#' Draw edges as Arcs
#'
#' This geom is mainly intended for arc linear and circular diagrams (i.e. used
#' together with \code{\link{layout_igraph_linear}}), though it can be used
#' elsewere. It draws edges as arcs with a hight proportional to the distance
#' between the nodes. Arcs are calculated as beziers. For linear layout the
#' placement of control points are related to the \code{curvature} argument and
#' the distance between the two nodes. For circular layout the control points
#' are placed on the same angle as the start and end node at a distance related
#' to the distance between the nodes.
#'
#' @details
#' Many geom_edge_* layers comes in 3 flavors depending on the level of control
#' needed over the drawing. The default (no numeric postfix) generate a number
#' of points (\code{n}) along the edge and draws it as a path. Each point along
#' the line has a numeric value associated with it giving the position along the
#' path, and it is therefore possible to show the direction of the edge by
#' mapping to this e.g. \code{colour = ..index..}. The version postfixed with a
#' "2" uses the "long" edge format (see \code{\link{get_edges}}) and makes it
#' possible to interpolate node parameter between the start and end node along
#' the edge. It is considerable less performant so should only be used if this
#' is needed. The version postfixed with a "0" draws the edge in the most
#' performant way, often directly using an appropriate grob from the grid
#' package, but does not allow for gradients along the edge.
#'
#' Often it is beneficial to stop the drawing of the edge before it reaches the
#' node, for instance in cases where an arrow should be drawn and the arrowhead
#' shouldn't lay ontop or below the node point. geom_edge_* and geom_edge_*2
#' supports this through the start_cap and end_cap aesthetics that takes a
#' \code{\link{geometry}} specification and dynamically caps the termini of the
#' edges based on the given specifications. This means that if
#' \code{end_cap = circle(1, 'cm')} the edges will end at a distance of 1cm even
#' during resizing of the plot window.
#'
#' All \code{geom_edge_*} and \code{geom_edge_*2} have the ability to draw a
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
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' \code{edge_colour} within the \code{aes()} call as \code{colour} will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_edge_arc and geom_edge_arc0 understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{xend}}
#'  \item{\strong{yend}}
#'  \item{\strong{circular}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_arc2 understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{group}}
#'  \item{\strong{circular}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_arc and geom_edge_arc2 furthermore takes the following
#' aesthetics.
#' \itemize{
#'   \item{start_cap}
#'   \item{end_cap}
#'   \item{label}
#'   \item{label_pos}
#'   \item{label_size}
#'   \item{angle}
#'   \item{hjust}
#'   \item{vjust}
#'   \item{family}
#'   \item{fontface}
#'   \item{lineheight}
#' }
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
#' @param curvature The bend of the curve. 1 approximates a halfcircle while 0
#' will give a straight line. Negative number will change the direction of the
#' curve. Only used if \code{circular = FALSE}.
#'
#' @param fold Logical. Should arcs appear on the same side of the nodes despite
#' different directions. Default to \code{FALSE}.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(igraph)
#' # Make a graph with different directions of edges
#' gr <- graph_from_edgelist(
#'   t(apply(as_edgelist(make_graph('Meredith')), 1, sample))
#' )
#' E(gr)$class <- sample(letters[1:3], gsize(gr), replace = TRUE)
#' V(gr)$class <- sample(letters[1:3], gorder(gr), replace = TRUE)
#'
#' ggraph(gr, 'linear') +
#'   geom_edge_arc(aes(alpha = ..index..))
#'
#' ggraph(gr, 'linear') +
#'   geom_edge_arc2(aes(colour = node.class), curvature = 0.6)
#'
#' ggraph(gr, 'linear', circular = TRUE) +
#'   geom_edge_arc0(aes(colour = class))
#'
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
        data$group <- seq_len(nrow(data))
        data2 <- data
        data2$x <- data2$xend
        data2$y <- data2$yend
        data$xend <- NULL
        data$yend <- NULL
        data2$xend <- NULL
        data2$yend <- NULL
        createArc(data, data2, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc <- function(mapping = NULL, data = get_edges(),
                          position = "identity", arrow = NULL, curvature = 1,
                          n = 100, fold = FALSE, lineend = "butt",
                          linejoin = "round", linemitre = 1,
                          label_colour = 'black',  label_alpha = 1,
                          label_parse = FALSE, check_overlap = FALSE,
                          angle_calc = 'rot', force_flip = TRUE,
                          label_dodge = NULL, label_push = NULL,
                          show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = FALSE, curvature = curvature, fold = fold,
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
StatEdgeArc2 <- ggproto('StatEdgeArc2', StatBezier2,
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        data <- data[order(data$group),]
        data2 <- data[c(FALSE, TRUE), ]
        data <- data[c(TRUE, FALSE), ]
        createArc(data, data2, params)
    },
    required_aes = c('x', 'y', 'group', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc2 <- function(mapping = NULL, data = get_edges('long'),
                           position = "identity", arrow = NULL, curvature = 1,
                           n = 100, fold = FALSE, lineend = "butt",
                           linejoin = "round", linemitre = 1,
                           label_colour = 'black',  label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge.id,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = TRUE, curvature = curvature, fold = fold,
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
StatEdgeArc0 <- ggproto('StatEdgeArc0', StatBezier0,
    setup_data = function(data, params) {
        StatEdgeArc$setup_data(data, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @export
geom_edge_arc0 <- function(mapping = NULL, data = get_edges(),
                           position = "identity", arrow = NULL, curvature = 1,
                           lineend = "butt", show.legend = NA, fold = fold, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                   curvature = curvature, fold = FALSE, ...)
          )
    )
}

createArc <- function(from, to, params) {
    bezierStart <- seq(1, by=4, length.out = nrow(from))
    from$index <- bezierStart
    to$index <- bezierStart + 3
    data2 <- from
    data3 <- to
    data2$index <- bezierStart + 1
    data3$index <- bezierStart + 2
    nodeDist <- sqrt((to$x - from$x)^2 + (to$y - from$y)^2) / 2
    circ <- from$circular
    if (any(circ)) {
        r0 <- sqrt(to$x[circ]^2 + to$y[circ]^2)
        r1 <- sqrt(to$x[circ]^2 + to$y[circ]^2)

        data2$x[circ] <- from$x[circ] * (1 - (nodeDist[circ]/r0))
        data2$y[circ] <- from$y[circ] * (1 - (nodeDist[circ]/r0))
        data3$x[circ] <- to$x[circ] * (1 - (nodeDist[circ]/r1))
        data3$y[circ] <- to$y[circ] * (1 - (nodeDist[circ]/r1))
    }
    if (any(!circ)) {
        curvature <- pi/2 * -params$curvature
        edgeAngle <- atan2(to$y[!circ] - from$y[!circ],
                           to$x[!circ] - from$x[!circ])
        startAngle <- edgeAngle - curvature
        endAngle <- edgeAngle - pi + curvature
        data2$x[!circ] <- data2$x[!circ] + cos(startAngle) * nodeDist[!circ]
        data2$y[!circ] <- data2$y[!circ] + sin(startAngle) * nodeDist[!circ]
        data3$x[!circ] <- data3$x[!circ] + cos(endAngle) * nodeDist[!circ]
        data3$y[!circ] <- data3$y[!circ] + sin(endAngle) * nodeDist[!circ]
        if (params$fold) {
            data2$x[!circ] <- abs(data2$x[!circ])
            data2$y[!circ] <- abs(data2$y[!circ])
            data3$x[!circ] <- abs(data3$x[!circ])
            data3$y[!circ] <- abs(data3$y[!circ])
        }
    }
    data <- rbind(from, data2, data3, to)
    data[order(data$index), names(data) != 'index']
}
