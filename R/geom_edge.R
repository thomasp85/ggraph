#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid segmentsGrob polylineGrob gpar
#' @importFrom dplyr %>% group_by_ do ungroup
#' @importFrom ggplot2 ggproto GeomPath alpha .pt zeroGrob
#' @importFrom ggforce interpolateDataFrame
#' @export
GeomEdgePath <- ggproto('GeomEdgePath', GeomPath,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE, interpolate = TRUE,
                          label_colour = 'black',  label_alpha = 1,
                          check_overlap = FALSE) {
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
        attr <- pathAttr(data, length(unique(data$group)))
        # attr <- data %>% group_by_(~group) %>%
        #     do({
        #         data.frame(solid = identical(unique(.$edge_linetype), 1),
        #                    constant = nrow(unique(.[, c("edge_alpha", "edge_colour",
        #                                                  "edge_width", "edge_linetype")])) == 1)
        #     }) %>%
        #     ungroup()

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
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 GeomSegment ggproto zeroGrob alpha aes
#' @importFrom grid gpar segmentsGrob
#' @export
GeomEdgeSegment <- ggproto('GeomEdgeSegment', GeomSegment,
    draw_panel = function(data, panel_scales, coord, arrow = NULL, lineend = "butt",
                          na.rm = FALSE) {
        if (is.null(data) || nrow(data) == 0 || ncol(data) == 0)
            return(zeroGrob())
        coord <- coord$transform(data, panel_scales)
        segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                     default.units = "native",
                     gp = gpar(col = alpha(coord$edge_colour, coord$edge_alpha),
                               fill = alpha(coord$edge_colour, coord$edge_alpha),
                               lwd = coord$edge_width * .pt,
                               lty = coord$edge_linetype,
                               lineend = lineend),
                     arrow = arrow)
    },
    draw_key = function(data, params, size) {
        segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = gpar(col = alpha(data$edge_colour, data$edge_alpha),
                               lwd = data$edge_width * .pt,
                               lty = data$edge_linetype, lineend = "butt"),
                     arrow = params$arrow)
    },
    default_aes = aes(edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
                      edge_alpha = NA)
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom ggforce GeomBezier0
#' @export
GeomEdgeBezier <- ggproto('GeomEdgeBezier', GeomBezier0,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE) {
        names(data) <- sub('edge_', '', names(data))
        names(data)[names(data) == 'width'] <- 'size'
        GeomBezier0$draw_panel(data, panel_scales, coord, arrow, lineend, linejoin, linemitre, na.rm)
    },
    draw_key = function(data, params, size) {
        segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = gpar(col = alpha(data$edge_colour, data$edge_alpha),
                               lwd = data$edge_width * .pt,
                               lty = data$edge_linetype, lineend = "butt"),
                     arrow = params$arrow)
    },
    default_aes = aes(edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
                      edge_alpha = NA),
    handle_na = function(data, ...) {
        data
    }
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom ggforce GeomBspline0
#' @export
GeomEdgeBspline <- ggproto('GeomEdgeBspline', GeomBspline0,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE) {
        names(data) <- sub('edge_', '', names(data))
        names(data)[names(data) == 'width'] <- 'size'
        GeomBspline0$draw_panel(data, panel_scales, coord, arrow, lineend, linejoin, linemitre, na.rm)
    },
    draw_key = function(data, params, size) {
        segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = gpar(col = alpha(data$edge_colour, data$edge_alpha),
                               lwd = data$edge_width * .pt,
                               lty = data$edge_linetype, lineend = "butt"),
                     arrow = params$arrow)
    },
    default_aes = aes(edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
                      edge_alpha = NA),
    handle_na = function(data, ...) {
        data
    }
)
