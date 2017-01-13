#' Draw edges as diagonals
#'
#' This geom draws edges as diagonal bezier curves as known from
#' \href{https://github.com/mbostock/d3/wiki/SVG-Shapes#_diagonal}{d3.svg.diagonal}.
#' A diagonal in this context is a quadratic bezier with the control points
#' positioned halfway between the start and end points but on the same axis.
#' This produces a pleasing fan-in, fan-out line that is mostly relevant for
#' heirarchical layouts as it implies an overall directionality in the plot.
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
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param flipped Logical, Has the layout been flipped by reassigning the
#' mapping of x, y etc?
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(igraph)
#' gr <- make_tree(20, 4, 'out')
#' E(gr)$class <- sample(letters[1:3], gsize(gr), replace = TRUE)
#' V(gr)$class <- sample(letters[1:3], gorder(gr), replace = TRUE)
#'
#' ggraph(gr, 'igraph', algorithm = 'tree') +
#'   geom_edge_diagonal(aes(alpha = ..index..))
#'
#' ggraph(gr, 'igraph', algorithm = 'tree') +
#'   geom_edge_diagonal2(aes(colour = node.class),
#'                       gEdges('long', nodePar = 'class'))
#'
#' ggraph(gr, 'igraph', algorithm = 'tree') +
#'   geom_edge_diagonal0(aes(colour = class))
#'
#' @rdname geom_edge_diagonal
#' @name geom_edge_diagonal
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeDiagonal <- ggproto('StatEdgeDiagonal', StatBezier,
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
        createDiagonal(data, data2, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'flipped', 'n')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal <- function(mapping = NULL, data = gEdges(),
                               position = "identity", arrow = NULL,
                               flipped = FALSE, lineend = "butt",
                               show.legend = NA, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeDiagonal,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = FALSE, flipped = flipped, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier2
#' @export
StatEdgeDiagonal2 <- ggproto('StatEdgeDiagonal2', StatBezier2,
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
        createDiagonal(data, data2, params)
    },
    required_aes = c('x', 'y', 'group', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'flipped', 'n')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal2 <- function(mapping = NULL, data = gEdges('long'),
                                position = "identity", arrow = NULL,
                                flipped = FALSE, lineend = "butt",
                                show.legend = NA, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge.id,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeDiagonal2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = TRUE, flipped = flipped, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeDiagonal0 <- ggproto('StatEdgeDiagonal0', StatBezier0,
    setup_data = function(data, params) {
        StatEdgeDiagonal$setup_data(data, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'flipped')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal0 <- function(mapping = NULL, data = gEdges(),
                                position = "identity", arrow = NULL,
                                flipped = FALSE, lineend = "butt",
                                show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          circular=~circular))
    layer(data = data, mapping = mapping, stat = StatEdgeDiagonal0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                        flipped = flipped, ...)
    )
}

createDiagonal <- function(from, to, params) {
    bezierStart <- seq(1, by=4, length.out = nrow(from))
    from$index <- bezierStart
    to$index <- bezierStart + 3
    data2 <- from
    data3 <- to
    data2$index <- bezierStart + 1
    data3$index <- bezierStart + 2
    if (any(from$circular)) {
        r0 <- sqrt(from$x[from$circular]^2 + from$y[from$circular]^2)
        r1 <- sqrt(to$x[to$circular]^2 + to$y[to$circular]^2)
        root <- r0 == 0 | r1 == 0
        rMid <- r0 + (r1 - r0)/2

        data2$x[from$circular] <- from$x[from$circular] / (r0/rMid)
        data2$y[from$circular] <- from$y[from$circular] / (r0/rMid)
        data3$x[from$circular] <- to$x[from$circular] / (r1/rMid)
        data3$y[from$circular] <- to$y[from$circular] / (r1/rMid)

        data2$x[root] <- from$x[root]
        data2$y[root] <- from$y[root]
        data3$x[root] <- to$x[root]
        data3$y[root] <- to$y[root]
    }
    if (any(!from$circular)) {
        if (params$flipped) {
            hDiff <- from$x[!from$circular] - to$x[!from$circular]
            data2$x[!from$circular] <- from$x[!from$circular]  - hDiff/2
            data3$x[!from$circular] <- to$x[!from$circular] + hDiff/2
        } else {
            hDiff <- from$y[!from$circular] - to$y[!from$circular]
            data2$y[!from$circular] <- from$y[!from$circular]  - hDiff/2
            data3$y[!from$circular] <- to$y[!from$circular] + hDiff/2
        }
    }
    data <- rbind(from, data2, data3, to)
    data[order(data$index), names(data) != 'index']
}
