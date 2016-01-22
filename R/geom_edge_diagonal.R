#' @importFrom ggplot2 ggproto
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
    extra_params = c('na.rm', 'flipped', 'n')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_diagonal <- function(mapping = NULL, data = gEdges(), stat = "edge_diagonal",
                               position = "identity", arrow = NULL, flipped = FALSE,
                               lineend = "butt", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = FALSE, flipped = flipped, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
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
    required_aes = c('x', 'y', 'group', 'circular', 'direction'),
    extra_params = c('na.rm', 'flipped', 'n')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_diagonal2 <- function(mapping = NULL, data = gEdges('long'), stat = "edge_diagonal2",
                             position = "identity", arrow = NULL, flipped = FALSE,
                             lineend = "butt", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, group=edge.id, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = TRUE, flipped = flipped, ...)
    )
}
#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeDiagonal0 <- ggproto('StatEdgeDiagonal0', StatBezier0,
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
    extra_params = c('na.rm', 'flipped')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_diagonal0 <- function(mapping = NULL, data = gEdges(), stat = "edge_diagonal0",
                             position = "identity", arrow = NULL, flipped = FALSE,
                             lineend = "butt", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeBezier,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, flipped = flipped, ...)
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
        r0 <- sqrt(to$x[to$circular]^2 + to$y[from$circular]^2)
        r1 <- sqrt(to$x[from$circular]^2 + to$y[from$circular]^2)
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
