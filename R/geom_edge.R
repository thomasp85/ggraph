#' @importFrom grid segmentsGrob polylineGrob gpar
#' @importFrom dplyr %>% group_by_ do ungroup
#' @importFrom ggplot2 ggproto GeomPath alpha .pt zeroGrob
#' @importFrom ggforce interpolateDataFrame
#' @export
GeomEdgePath <- ggproto('GeomEdgePath', GeomPath,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE, interpolate = TRUE) {
        if (!anyDuplicated(data$group)) {
            message("geom_edge_path: Each group consists of only one observation. ",
                         "Do you need to adjust the group aesthetic?")
        }
        data <- data[order(data$group), , drop = FALSE]
        if (interpolate) {
            data <- interpolateDataFrame(data)
        }
        data <- coord$transform(data, panel_scales)
        if (nrow(data) < 2) return(zeroGrob())
        attr <- data %>% group_by_(~group) %>%
            do({
                data.frame(solid = identical(unique(.$edge_linetype), 1),
                           constant = nrow(unique(.[, c("edge_alpha", "edge_colour",
                                                         "edge_width", "edge_linetype")])) == 1)
            }) %>%
            ungroup()

        solid_lines <- all(attr$solid)
        constant <- all(attr$constant)
        if (!solid_lines && !constant) {
            stop("geom_edge_path: If you are using dotted or dashed lines",
                 ", colour, size and linetype must be constant over the line",
                 call. = FALSE)
        }
        n <- nrow(data)
        group_diff <- data$group[-1] != data$group[-n]
        start <- c(TRUE, group_diff)
        end <- c(group_diff, TRUE)
        if (!constant) {
            segmentsGrob(data$x[!end], data$y[!end], data$x[!start],
                         data$y[!start], default.units = "native", arrow = arrow,
                         gp = gpar(col = alpha(data$edge_colour, data$edge_alpha)[!end],
                                   fill = alpha(data$edge_colour, data$edge_alpha)[!end],
                                   lwd = data$edge_width[!end] * .pt, lty = data$edge_linetype[!end],
                                   lineend = lineend, linejoin = linejoin, linemitre = linemitre))
        }
        else {
            id <- match(data$group, unique(data$group))
            polylineGrob(data$x, data$y, id = id, default.units = "native",
                         arrow = arrow,
                         gp = gpar(col = alpha(data$edge_colour, data$edge_alpha)[start],
                                   fill = alpha(data$edge_colour, data$edge_alpha)[start],
                                   lwd = data$edge_width[start] * .pt,
                                   lty = data$edge_linetype[start],
                                   lineend = lineend, linejoin = linejoin,
                                   linemitre = linemitre))
        }
    },
    draw_key = function (data, params, size) {
        segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = gpar(col = alpha(data$edge_colour, data$edge_alpha),
                               lwd = data$edge_width * .pt,
                               lty = data$edge_linetype, lineend = "butt"),
                     arrow = params$arrow)
    },
    handle_na = function(data, params) {
        if (params$interpolate) return(data)
        keep <- function(x) {
            first <- match(FALSE, x, nomatch = 1) - 1
            last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
            c(rep(FALSE, first),
              rep(TRUE, last - first),
              rep(FALSE, length(x) - last))
        }
        missing <- !stats::complete.cases(
            data[c("x", "y", "edge_width", "edge_colour", "edge_linetype")]
        )
        kept <- stats::ave(missing, data$group, FUN = keep)
        data <- data[kept, ]
        if (!all(kept) && !params$na.rm) {
            warning("Removed ", sum(!kept), " rows containing missing values",
                    " (geom_edge_path).", call. = FALSE)
        }
        data
    },
    default_aes = aes(edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
                    edge_alpha = NA)
)
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
#' @export
gEdges <- function(format = 'short', nodeFilter = NULL, nodePar = NULL) {
    function(layout) {
        edges <- getEdges(layout)
        if (!is.null(nodeFilter)) {
            keep <- with(
                list(node1 = layout[edges$from, ], node2 = layout[edges$to, ]),
                nodeFilter
            )
            edges <- edges[keep, ]
        }
        edges <- switch(
            format,
            short = formatShortEdges(edges, layout, nodePar),
            long = formatLongEdges(edges, layout, nodePar),
            stop('Unknown format. Use either "short" or "long"')
        )
        edges
    }
}
formatShortEdges <- function(edges, layout, nodePar) {
    edges <- addEdgeCoordinates(edges, layout)
    if (!is.null(nodePar)) {
        nodes1 <- layout[edges$from, nodePar, drop = FALSE]
        names(nodes1) <- paste0('node1.', names(nodes1))
        nodes2 <- layout[edges$to, nodePar, drop = FALSE]
        names(nodes2) <- paste0('node2.', names(nodes2))
        edges <- cbind(edges, nodes1, nodes2)
    }
    rownames(edges) <- NULL
    checkShortEdges(edges)
}
formatLongEdges <- function(edges, layout, nodePar) {
    from <- cbind(edge.id = seq_len(nrow(edges)),
                  node = edges$from,
                  layout[edges$from, c('x', 'y')],
                  edges[, !names(edges) %in% c('from', 'to'), drop = FALSE])
    to <- cbind(edge.id = seq_len(nrow(edges)),
                node = edges$to,
                layout[edges$to, c('x', 'y')],
                edges[, !names(edges) %in% c('from', 'to'), drop = FALSE])
    edges <- rbind(from, to)
    if (!is.null(nodePar)) {
        node <- layout[edges$node, nodePar, drop = FALSE]
        names(node) <- paste0('node.', names(node))
        edges <- cbind(edges, node)
    }
    rownames(edges) <- NULL
    edges[order(edges$edge.id), ]
}
completeEdgeAes <- function(aesthetics) {
    if (is.null(aesthetics)) return(aesthetics)
    if (any(names(aesthetics) == 'color')) {
        names(aesthetics)[names(aesthetics) == 'color'] <- 'colour'
    }
    shortNames <- names(aesthetics) %in% c(
        'colour', 'fill', 'linetype', 'shape', 'size', 'width'
    )
    names(aesthetics)[shortNames] <- paste0('edge_', names(aesthetics)[shortNames])
    aesthetics
}
