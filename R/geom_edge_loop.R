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
#' geom_edge_loop and geom_edge_loop0 understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{from}}
#'  \item{\strong{to}}
#'  \item{\strong{span} \emph{90}}
#'  \item{\strong{direction} \emph{45}}
#'  \item{\strong{strength} \emph{1}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_loop furthermore takes the following
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
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(igraph)
#' gr <- graph_from_data_frame(
#'   data.frame(from=c(1,1,2,2,3,3,3), to=c(1,2,2,3,3,1,2))
#' )
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_loop(aes(alpha = ..index..)) +
#'   geom_edge_fan(aes(alpha = ..index..))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_loop0() +
#'   geom_edge_fan0()
#'
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
        data <- data[data$from == data$to, ]
        data$group <- seq_len(nrow(data))
        if (nrow(data) != 0) {
            createLoops(data, params)
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
                           position = "identity", arrow = NULL, n = 100,
                           lineend = "butt", linejoin = "round", linemitre = 1,
                           label_colour = 'black',  label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, from=~from, to=~to,
                                          span=90, direction=45, strength=1))
    layer(data = data, mapping = mapping, stat = StatEdgeLoop,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = FALSE,
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
                            position = "identity", arrow = NULL,
                            lineend = "butt", show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, from=~from, to=~to,
                                          span=90, direction=45, strength=1))
    layer(data = data, mapping = mapping, stat = StatEdgeLoop0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE, ...)
          )
    )
}

createLoops <- function(loops, params) {
    controlAngle1 <- loops$direction - loops$span/2
    controlAngle2 <- loops$direction + loops$span/2
    controls1 <- findLoopControls(loops, controlAngle1)
    controls2 <- findLoopControls(loops, controlAngle2)
    end <- loops
    bezierStart <- seq(1, by = 4, length.out = nrow(loops))
    loops$index <- bezierStart
    controls1$index <- bezierStart + 1
    controls2$index <- bezierStart + 2
    end$index <- bezierStart + 3
    loops <- rbind(loops, controls1, controls2, end)
    loops[order(loops$index), names(loops) != 'index']
}
findLoopControls <- function(loops, angle) {
    angle <- angle/360 * 2*pi
    loops$x <- loops$x + cos(angle) * loops$strength
    loops$y <- loops$y + sin(angle) * loops$strength
    loops
}
