#' @importFrom ggplot2 ggproto Stat
#' @importFrom ggforce radial_trans
#' @export
StatEdgeElbow <- ggproto('StatEdgeElbow', Stat,
    compute_panel = function(data, scales, flipped = FALSE, n = 100) {
        if (n %% 2) n <- n + 1
        index <- seq(0, 1, length.out = n)
        if (any(data$circular)) {
            circId <- which(data$circular)
            dataCirc <- data[circId, ]
            radial <- radial_trans(c(0, 1), c(2*pi, 0), pad = 0, offset = 0)
            start <- atan2(dataCirc$y, dataCirc$x)
            radiiStart <- sqrt(dataCirc$x^2 + dataCirc$y^2)
            radiiEnd <- sqrt(dataCirc$xend^2 + dataCirc$yend^2)
            angelDiff <- (dataCirc$x*dataCirc$xend + dataCirc$y*dataCirc$yend) /
             (radiiStart*radiiEnd)
            angelDiff[is.nan(angelDiff)] <- 0
            angelDiff <- suppressWarnings(acos(angelDiff))
            angelDiff[is.nan(angelDiff)] <- 0
            end <- start + ifelse(dataCirc$direction == 'left', -angelDiff, angelDiff)
            angles <- unlist(Map(seq, from = start, to = end, length.out = n/2))
            radii <- rep(sqrt(data$y[circId]^2 + data$x[circId]^2), each = n/2)
            pathCirc <- radial$transform(r = radii, a = angles)
            pathCirc$group <- rep(circId, each = n/2)
            pathCirc$index <- rep(index[seq_len(n/2)], length(circId))
            radiiRel <- radiiStart / radiiEnd
            elbowX <- dataCirc$xend * radiiRel
            elbowY <- dataCirc$yend * radiiRel
            elbowX <- unlist(Map(seq, from = elbowX, to = dataCirc$xend, length.out = n/2))
            elbowY <- unlist(Map(seq, from = elbowY, to = dataCirc$yend, length.out = n/2))
            pathCirc <- rbind(pathCirc,
                              data.frame(x = elbowX,
                                         y = elbowY,
                                         group = pathCirc$group,
                                         index = rep(index[seq_len(n/2) + n/2], length(circId))))
            pathCirc <- cbind(pathCirc, data[pathCirc$group, !names(data) %in% c('x', 'y', 'xend', 'yend')])
        }
        if (any(!data$circular)) {
            pathLin <- lapply(which(!data$circular), function(i) {
                if (flipped) {
                    path <- data.frame(
                        x = approx(c(data$x[i], data$x[i], data$xend[i]), n = n)$y,
                        y = approx(c(data$y[i], data$yend[i], data$yend[i]), n = n)$y,
                        group = i,
                        index = index
                    )
                } else {
                    path <- data.frame(
                        x = approx(c(data$x[i], data$xend[i], data$xend[i]), n = n)$y,
                        y = approx(c(data$y[i], data$y[i], data$yend[i]), n = n)$y,
                        group = i,
                        index = index
                    )
                }
                cbind(path, data[rep(i, nrow(path)), !names(data) %in% c('x', 'y', 'xend', 'yend')])
            })
            pathLin <- do.call(rbind, pathLin)

            if (any(data$circular)) {
                paths <- rbind(pathLin, pathCirc)
            } else {
                paths <- pathLin
            }
        } else {
            paths <- pathCirc
        }
        paths[order(paths$group), ]
    },
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        data
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular', 'direction')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_elbow <- function(mapping = NULL, data = gEdges(), stat = "edge_elbow",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = FALSE, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
#' @export
StatEdgeElbow2 <- ggproto('StatEdgeElbow2', Stat,
    compute_panel = function(data, scales, flipped = FALSE, n = 100) {
        posCols <- c('x', 'y', 'group', 'circular', 'direction', 'PANEL')
        data <- data[order(data$group), ]
        posData <- cbind(data[c(TRUE, FALSE), posCols], data[c(FALSE, TRUE), c('x', 'y')])
        names(posData) <- c(posCols, 'xend', 'yend')
        newData <- StatEdgeElbow$compute_panel(posData, scales, flipped, n)
        extraCols <- !names(data) %in% posCols
        index <- match(seq_len(nrow(posData)), newData$group)
        index <- as.vector(rbind(index, index + 1))
        newData$.interp <- TRUE
        newData$.interp[index] <- FALSE
        if (sum(extraCols) != 0) {
            for (i in names(data)[extraCols]) {
                newData[[i]] <- NA
                newData[[i]][index] <- data[[i]]
            }
        }
        newData
    },
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        data
    },
    required_aes = c('x', 'y', 'group', 'circular', 'direction')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_elbow2 <- function(mapping = NULL, data = gEdges('long'), stat = "edge_elbow2",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, n = 100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, group=edge.id, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, interpolate = TRUE, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
#' @export
StatEdgeElbow0 <- ggproto('StatEdgeElbow0', Stat,
    compute_panel = function(data, scales, flipped = FALSE) {
        if (any(data$circular)) {
            circId <- which(data$circular)
            dataCirc <- data[circId, ]
            radial <- radial_trans(c(0, 1), c(2*pi, 0), pad = 0, offset = 0)
            start <- atan2(dataCirc$y, dataCirc$x)
            angelDiff <- (dataCirc$x*dataCirc$xend + dataCirc$y*dataCirc$yend) /
                (sqrt(dataCirc$x^2 + dataCirc$y^2)*sqrt(dataCirc$xend^2 + dataCirc$yend^2))
            angelDiff[is.nan(angelDiff)] <- 0
            angelDiff <- suppressWarnings(acos(angelDiff))
            angelDiff[is.nan(angelDiff)] <- 0
            end <- start + ifelse(dataCirc$direction == 'left', -angelDiff, angelDiff)
            angles <- unlist(Map(seq, from = start, to = end, length.out = 50))
            radii <- rep(sqrt(data$y[circId]^2 + data$x[circId]^2), each = 50)
            pathCirc <- radial$transform(r = radii, a = angles)
            pathCirc$group <- rep(circId, each = 50)
            pathCirc <- rbind(pathCirc,
                              data.frame(x = data$xend[circId],
                                         y = data$yend[circId],
                                         group = circId))
            pathCirc <- cbind(pathCirc, data[pathCirc$group, !names(data) %in% c('x', 'y', 'xend', 'yend')])
        }
        if (any(!data$circular)) {
            pathLin <- lapply(which(!data$circular), function(i) {
                if (flipped) {
                    path <- data.frame(
                        x = c(data$x[i], data$x[i], data$xend[i]),
                        y = c(data$y[i], data$yend[i], data$yend[i]),
                        group = i
                    )
                } else {
                    path <- data.frame(
                        x = c(data$x[i], data$xend[i], data$xend[i]),
                        y = c(data$y[i], data$y[i], data$yend[i]),
                        group = i
                    )
                }
                cbind(path, data[rep(i, nrow(path)), !names(data) %in% c('x', 'y', 'xend', 'yend')])
            })
            pathLin <- do.call(rbind, pathLin)

            if (any(data$circular)) {
                paths <- rbind(pathLin, pathCirc)
            } else {
                paths <- pathLin
            }
        } else {
            paths <- pathCirc
        }
        paths[order(paths$group), ]
    },
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        data
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'circular', 'direction')
)
#' @importFrom ggplot2 layer aes
#' @export
geom_edge_elbow0 <- function(mapping = NULL, data = gEdges(), stat = "edge_elbow0",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgePath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, interpolate = FALSE, ...)
    )
}
