#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid segmentsGrob polylineGrob gpar
#' @importFrom dplyr %>% group_by_ do ungroup
#' @importFrom ggforce interpolateDataFrame
#' @export
GeomEdgePath <- ggproto('GeomEdgePath', GeomPath,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE, interpolate = TRUE,
                          label_colour = 'black',  label_alpha = 1, label_parse = FALSE,
                          check_overlap = FALSE, angle_calc = 'none', force_flip = TRUE,
                          label_dodge = NULL, label_push = NULL) {
        if (!anyDuplicated(data$group)) {
            message("geom_edge_path: Each group consists of only one observation. ",
                         "Do you need to adjust the group aesthetic?")
        }
        data <- data[order(data$group), , drop = FALSE]
        if (interpolate) {
            data <- interpolateDataFrame(data)
        }
        data <- coord$transform(data, panel_scales)
        zero <- coord$transform(data.frame(x=0, y=0), panel_scales)
        if (nrow(data) < 2) return(zeroGrob())
        attr <- pathAttr(data, length(unique(data$group)))

        if (all(is.na(data$start_cap))) {
            start_captype <- 'circle'
            start_cap <- NULL
            start_cap2 <- NULL
        } else {
            scap <- data$start_cap[!duplicated(data$group)]
            start_captype <- geo_type(scap)
            start_cap <- geo_width(scap)
            start_cap2 <- geo_height(scap)
            if (any(attr(start_cap, 'unit') == 'native') ||
                any(attr(start_cap2, 'unit') == 'native')) {
                recalc <- coord$transform(
                    data.frame(x=as.vector(start_cap), y=as.vector(start_cap2)),
                    panel_scales
                )
                start_cap[attr(start_cap, 'unit') == 'native'] <- unit(recalc$x-zero$x, 'npc')
                start_cap2[attr(start_cap2, 'unit') == 'native'] <- unit(recalc$y-zero$y, 'npc')
            }
        }
        if (all(is.na(data$end_cap))) {
            end_captype <- 'circle'
            end_cap <- NULL
            end_cap2 <- NULL
        } else {
            ecap <- data$end_cap[!duplicated(data$group)]
            end_captype <- geo_type(ecap)
            end_cap <- geo_width(ecap)
            end_cap2 <- geo_height(ecap)
            if (any(attr(end_cap, 'unit') == 'native') ||
                any(attr(end_cap2, 'unit') == 'native')) {
                recalc <- coord$transform(
                    data.frame(x=as.vector(end_cap), y=as.vector(end_cap2)),
                    panel_scales
                )
                end_cap[attr(end_cap, 'unit') == 'native'] <- unit(recalc$x-zero$x, 'native')
                end_cap2[attr(end_cap2, 'unit') == 'native'] <- unit(recalc$y-zero$y, 'native')
            }
        }
        if ((is.null(start_cap) && !is.null(end_cap)) ||
            (!is.null(start_cap) && is.null(end_cap))) {
            if (is.null(start_cap)) start_cap <- 0
            if (is.null(end_cap)) end_cap <- 0
        }

        solid_lines <- all(attr$solid)
        constant <- all(attr$constant)
        if (!solid_lines && !constant) {
            stop("geom_edge_path: If you are using dotted or dashed lines",
                 ", colour, size and linetype must be constant over the line",
                 call. = FALSE)
        }
        gp <- gpar(col = alpha(data$edge_colour, data$edge_alpha),
                   fill = alpha(data$edge_colour, data$edge_alpha),
                   lwd = data$edge_width * .pt, lty = data$edge_linetype,
                   lineend = lineend, linejoin = linejoin, linemitre = linemitre)
        edgeGrob <- cappedPathGrob(
            x = data$x, y = data$y, id=data$group, arrow = arrow,
            start.cap = start_cap, start.cap2 = start_cap2, start.captype = start_captype,
            end.cap = end_cap, end.cap2 = end_cap2, end.captype = end_captype,
            default.units="native", gp=gp, constant = constant
        )
        if (any(!is.na(data$label))) {
            if (angle_calc == 'none') angle_calc <- 'rot'
            edge_ind <- split(seq_len(nrow(data)), data$group)
            edge_length <- lengths(edge_ind)
            edge_start <- c(0, cumsum(edge_length)[-length(edge_length)]) + 1
            label_pos <- data$label_pos[sapply(edge_ind, head, n = 1)]
            label_pos <- 1 + floor((edge_length - 1) * label_pos)
            label_ind <- edge_start + label_pos
            label_data <- data[label_ind, ]
            label_data$label_colour <- if (is.na(label_colour)) {
                label_data$edge_colour
            } else {
                label_colour
            }
            label_data$label_alpha <- if (is.na(label_alpha)) {
                label_data$edge_alpha
            } else {
                label_alpha
            }
            angle_start <- ifelse(label_pos == 1, 1, label_pos - 1)
            angle_end <- ifelse(label_pos == edge_length, edge_length, label_pos + 1)
            label_x0 <- data$x[angle_start + edge_start]
            label_y0 <- data$y[angle_start + edge_start]
            label_x1 <- data$x[angle_end + edge_start]
            label_y1 <- data$y[angle_end + edge_start]
            lab <- label_data$label
            if (label_parse) {
                lab <- parse(text = as.character(lab))
            }
            labelGrob <- textAlongGrob(
                lab, label_data$x, label_data$y, default.units = "native",
                hjust = label_data$hjust, vjust = label_data$vjust,
                rot = label_data$angle,
                gp = gpar(
                    col = alpha(label_colour, label_alpha),
                    fontsize = label_data$label_size * .pt,
                    fontfamily = label_data$family,
                    fontface = label_data$fontface,
                    lineheight = label_data$lineheight
                ),
                check.overlap = check_overlap, rot.type = angle_calc,
                x0 = label_x0, y0 = label_y0, x1 = label_x1, y1 = label_y1,
                force.rot = force_flip, dodge = label_dodge, push = label_push
            )
            gList(edgeGrob, labelGrob)
        } else {
            edgeGrob
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
    default_aes = aes(edge_colour = 'black', edge_width = 0.5, edge_linetype = 'solid',
                      edge_alpha = NA, start_cap = NA, end_cap = NA, label = NA,
                      label_pos = 0.5, label_size = 3.88, angle = 0,
                      hjust = 0.5, vjust = 0.5, family = '', fontface = 1,
                      lineheight = 1.2)
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
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
