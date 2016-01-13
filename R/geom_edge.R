#' @importFrom ggplot2 layer GeomSegment aes
#' @export
geom_edge_link <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", arrow = NULL,
                           lineend = "butt", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
    if (is.null(data)) data <- getEdges
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSegment,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
#' @export
StatEdgeElbow <- ggproto('StatEdgeElbow', Stat,
    compute_panel = function(data, scales, flipped = FALSE) {
        data = data[data$filter, ]

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
                rbind(pathLin, pathCirc)
            } else {
                pathLin
            }
        } else {
            pathCirc
        }
    },

    required_aes = c('x', 'y', 'xend', 'yend', 'filter', 'circular', 'direction')
)
#' @importFrom ggplot2 layer aes
#' @export
stat_edge_elbow <- function(mapping = NULL, data = NULL, geom = "path",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    if (is.null(data)) data <- getEdges
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, filter=TRUE, circular=circular, direction=direction))
    layer(
        stat = StatEdgeElbow, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 layer GeomPath aes
#' @export
geom_edge_elbow <- function(mapping = NULL, data = NULL, stat = "edge_elbow",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    if (is.null(data)) data <- getEdges
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, filter=TRUE, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
#' @export
StatEdgeCurve <- ggproto('StatEdgeCurve', Stat,
    compute_panel = function(data, scales) {
        data[data$filter, ]
    },

    required_aes = c('x', 'y', 'xend', 'yend', 'filter', 'circular', 'direction')
)
#' @importFrom ggplot2 layer aes
#' @export
stat_edge_curve <- function(mapping = NULL, data = NULL, geom = "edge_curve",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    if (is.null(data)) data <- getEdges
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, filter=TRUE, circular=circular, direction=direction))
    layer(
        stat = StatEdgeCurve, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 ggproto GeomCurve
#' @importFrom grid gList
GeomEdgeCurve <- ggproto('GeomEdgeCurve', GeomCurve,
    draw_panel = function(data, panel_scales, coord, curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, lineend = 'butt', na.rm = FALSE) {
        dataSplit <- split(seq_len(nrow(data)), (data$direction == 'left' & !data$circular) | (data$direction == 'right' & data$circular))
        gList(
            GeomCurve$draw_panel(data[dataSplit[['TRUE']], ], panel_scales, coord, -curvature, angle, ncp, arrow, lineend, na.rm),
            GeomCurve$draw_panel(data[dataSplit[['FALSE']], ], panel_scales, coord, curvature, angle, ncp, arrow, lineend, na.rm)
        )
    }
)
#' @importFrom ggplot2 layer GeomPath aes
#' @export
geom_edge_curve <- function(mapping = NULL, data = NULL, stat = "edge_curve",
                            position = "identity", arrow = NULL,
                            lineend = "butt", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
    if (is.null(data)) data <- getEdges
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend, filter=TRUE, circular=circular, direction=direction))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeCurve,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
    )
}
