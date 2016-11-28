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
#'  \item{\strong{xend}}
#'  \item{\strong{yend}}
#'  \item{\strong{circular}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_diagonal2 understand the following aesthetics. Bold aesthetics are
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
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x, y, xend, yend, group and
#' circular are mapped to x, y, xend, yend, edge.id and circular in the edge
#' data.
#'
#' @param data The return of a call to \code{gEdges()} or a data.frame
#' giving edges in corrent format (see details for for guidance on the format).
#' See \code{\link{gEdges}} for more details on edge extraction.
#'
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function. Currently no meaningful position
#' adjustment exists for edges.
#'
#' @param n The number of points to create along the path.
#'
#' @param curvature The bend of the curve. 1 approximates a halfcircle while 0
#' will give a straight line. Negative number will change the direction of the
#' curve. Only used if \code{circular = FALSE}.
#'
#' @param arrow Arrow specification, as created by \code{\link[grid]{arrow}}
#'
#' @param lineend Line end style (round, butt, square)
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#' are three types of arguments you can use here:
#' \itemize{
#'  \item{Aesthetics: to set an aesthetic to a fixed value, like
#'  \code{color = "red"} or \code{size = 3.}}
#'  \item{Other arguments to the layer, for example you override the default
#'  \code{stat} associated with the layer.}
#'  \item{Other arguments passed on to the stat.}
#' }
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#' never includes, and \code{TRUE} always includes.
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
#'   geom_edge_arc2(aes(colour = node.class),
#'                  gEdges('long', nodePar = 'class'), curvature = 0.6)
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
#' @importFrom ggplot2 ggproto
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
    extra_params = c('na.rm', 'n', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_arc <- function(mapping = NULL, data = gEdges(),
                          position = "identity", arrow = NULL, curvature = 1,
                          lineend = "butt", show.legend = NA, n = 100, fold = FALSE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = FALSE, curvature = curvature, fold = fold, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
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
    extra_params = c('na.rm', 'n', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_arc2 <- function(mapping = NULL, data = gEdges('long'),
                           position = "identity", arrow = NULL, curvature = 1,
                           lineend = "butt", show.legend = NA, n = 100, fold = FALSE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge.id,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = TRUE, curvature = curvature, fold = fold, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeArc0 <- ggproto('StatEdgeArc0', StatBezier0,
    setup_data = function(data, params) {
        StatEdgeArc$setup_data(data, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
    extra_params = c('na.rm', 'curvature', 'fold')
)
#' @rdname geom_edge_arc
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_arc0 <- function(mapping = NULL, data = gEdges(),
                           position = "identity", arrow = NULL, curvature = 1,
                           lineend = "butt", show.legend = NA, fold = fold, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeArc0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                        curvature = curvature, fold = FALSE, ...)
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
