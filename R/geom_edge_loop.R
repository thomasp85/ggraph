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
#' "2" uses the "long" edge format (see \code{\link{gEdges}}) and makes it
#' possible to interpolate node parameter between the start and end node along
#' the edge. It is considerable less performant so should only be used if this
#' is needed. The version postfixed with a "0" draws the edge in the most
#' performant way, often directly using an appropriate grob from the grid
#' package, but does not allow for gradients along the edge.
#'
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' \code{edge_colour} within the \code{aes()} call as \code{colour} will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_edge_diagonal and geom_edge_diagonal0 understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{from}}
#'  \item{\strong{to}}
#'  \item{\strong{angle} \emph{90}}
#'  \item{\strong{direction} \emph{45}}
#'  \item{\strong{strength} \emph{1}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
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
    required_aes = c('x', 'y', 'from', 'to', 'angle', 'direction', 'strength'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n')
)
#' @rdname geom_edge_loop
#'
#' @export
geom_edge_loop <- function(mapping = NULL, data = gEdges(),
                           position = "identity", arrow = NULL,
                           lineend = "butt", show.legend = NA, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, from=~from, to=~to,
                                          angle=90, direction=45, strength=1))
    layer(data = data, mapping = mapping, stat = StatEdgeLoop,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = FALSE, ...)
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
    required_aes = c('x', 'y', 'from', 'to', 'angle', 'direction', 'strength'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm')
)
#' @rdname geom_edge_loop
#'
#' @export
geom_edge_loop0 <- function(mapping = NULL, data = gEdges(),
                            position = "identity", arrow = NULL,
                            lineend = "butt", show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, from=~from, to=~to,
                                          angle=90, direction=45, strength=1))
    layer(data = data, mapping = mapping, stat = StatEdgeLoop0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE, ...)
          )
    )
}

createLoops <- function(loops, params) {
    controlAngle1 <- loops$direction - loops$angle/2
    controlAngle2 <- loops$direction + loops$angle/2
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
