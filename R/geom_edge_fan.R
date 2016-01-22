#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatBezier
#' @export
StatEdgeFan <- ggproto('StatEdgeFan', StatBezier,
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
        createFans(data, data2, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
    extra_params = c('na.rm', 'n', 'spread')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_fan <- function(mapping = NULL, data = gEdges(), stat = "edge_fan",
                               position = "identity", arrow = NULL,
                               lineend = "butt", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, n = 100, spread = 1, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, from=from, to=to))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = FALSE, spread = spread, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
#' @importFrom ggforce StatBezier2
#' @export
StatEdgeFan2 <- ggproto('StatEdgeFan2', StatBezier2,
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
        createFans(data, data2, params)
    },
    required_aes = c('x', 'y', 'group', 'from', 'to'),
    extra_params = c('na.rm', 'n', 'spread')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_fan2 <- function(mapping = NULL, data = gEdges('long'), stat = "edge_fan2",
                                position = "identity", arrow = NULL, spread = 1,
                                lineend = "butt", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, group=edge.id, from=from, to=to))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = TRUE, spread = spread, ...)
    )
}
#' @importFrom ggplot2 ggproto
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeFan0 <- ggproto('StatEdgeFan0', StatBezier0,
    setup_data = function(data, params) {
        browser()
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
        createFans(data, data2, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
    extra_params = c('na.rm', 'spread')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_fan0 <- function(mapping = NULL, data = gEdges(), stat = "edge_fan0",
                                position = "identity", arrow = NULL, spread = 1,
                                lineend = "butt", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, from=from, to=to))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeBezier,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, spread = spread, ...)
    )
}
#' @importFrom dplyr %>% group_by_ arrange_ mutate_ n ungroup transmute_
createFans <- function(from, to, params) {
    from$.id <- paste(pmin(from$from, to$to), pmax(from$from, to$to), sep = '-')
    from$.origInd <- seq_len(nrow(from))
    position <- from %>% group_by_(~.id) %>%
        arrange_(~from) %>%
        mutate_(position = ~seq_len(n()) - 0.5 - n()/2) %>%
        ungroup() %>%
        arrange_(~.origInd) %>%
        transmute_(position = ~position)
    position <- position$position
    maxFans <- max(table(from$.id))
    from$.id <- NULL
    from$.origInd <- NULL
    meanX <- rowMeans(cbind(from$x, to$x))
    meanY <- rowMeans(cbind(from$y, to$y))
    stepX <- -(params$spread * (to$y - from$y) / (2*maxFans))
    stepY <- params$spread * (to$x - from$x) / (2*maxFans)
    data <- from
    data$x <- meanX + stepX*position
    data$y <- meanY + stepY*position
    bezierStart <- seq(1, by = 3, length.out = nrow(from))
    from$index <- bezierStart
    to$index <- bezierStart + 2
    data$index <- bezierStart + 1
    data <- rbind(from, data, to)
    data[order(data$index), names(data) != 'index']
}
