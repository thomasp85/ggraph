#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid segmentsGrob polylineGrob gpar
#' @importFrom dplyr %>% group_by_ do ungroup
#' @importFrom ggforce interpolateDataFrame
#' @export
GeomEdgePath <- ggproto('GeomEdgePath', GeomPath,
  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                        lineend = 'butt', linejoin = 'round', linemitre = 1,
                        na.rm = FALSE, interpolate = TRUE,
                        label_colour = 'black', label_alpha = 1, label_parse = FALSE,
                        check_overlap = FALSE, angle_calc = 'none', force_flip = TRUE,
                        label_dodge = NULL, label_push = NULL) {
    if (!anyDuplicated(data$group)) {
      message(
        'geom_edge_path: Each group consists of only one observation. ',
        'Do you need to adjust the group aesthetic?'
      )
    }
    data <- data[order(data$group), , drop = FALSE]
    data$group <- match(data$group, unique(data$group))
    if (interpolate) {
      geometries <- vapply(data, is.geometry, logical(1))
      geom_cols <- data[geometries]
      data <- cbind(interpolateDataFrame(data[!geometries]), geom_cols)
    }
    data <- coord$transform(data, panel_scales)
    zero <- coord$transform(new_data_frame(list(x = 0, y = 0)), panel_scales)
    if (nrow(data) < 2) {
      return(zeroGrob())
    }
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
      native_start <- grepl('native', as.character(start_cap))
      native_start2 <- grepl('native', as.character(start_cap2))
      if (any(native_start) || any(native_start2)) {
        recalc <- coord$transform(
          new_data_frame(list(x = as.vector(start_cap), y = as.vector(start_cap2))),
          panel_scales
        )
        start_cap[native_start] <- unit(recalc$x - zero$x, 'npc')[native_start]
        start_cap2[native_start2] <- unit(recalc$y - zero$y, 'npc')[native_start2]
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
      native_end <- grepl('native', as.character(end_cap))
      native_end2 <- grepl('native', as.character(end_cap2))
      if (any(native_end) || any(native_end2)) {
        recalc <- coord$transform(
          new_data_frame(list(x = as.vector(end_cap), y = as.vector(end_cap2))),
          panel_scales
        )
        end_cap[native_end] <- unit(recalc$x - zero$x, 'native')[native_end]
        end_cap2[native_end2] <- unit(recalc$y - zero$y, 'native')[native_end2]
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
      stop('geom_edge_path: If you are using dotted or dashed lines',
        ', colour, size and linetype must be constant over the line',
        call. = FALSE
      )
    }
    gp <- gpar(
      col = alpha(data$edge_colour, data$edge_alpha),
      fill = alpha(data$edge_colour, data$edge_alpha),
      lwd = data$edge_width * .pt, lty = data$edge_linetype,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    edge_grob <- cappedPathGrob(
      x = data$x, y = data$y, id = data$group, arrow = arrow,
      start.cap = start_cap, start.cap2 = start_cap2, start.captype = start_captype,
      end.cap = end_cap, end.cap2 = end_cap2, end.captype = end_captype,
      default.units = 'native', gp = gp, constant = constant
    )
    if (any(!is.na(data$label))) {
      if (angle_calc == 'none') angle_calc <- 'rot'
      edge_ind <- split(seq_len(nrow(data)), data$group)
      edge_length <- lengths(edge_ind)
      edge_start <- c(0, cumsum(edge_length)[-length(edge_length)]) + 1
      label_pos <- data$label_pos[vapply(edge_ind, head, integer(1), n = 1)]
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
      lab <- as.character(label_data$label)
      if (label_parse) {
        lab_expressions <- sapply(X=lab, FUN=str2expression)
        lab_len <- sapply(X=lab_expressions, FUN=length)
        if (any(lab_len == 0)) {
          lab[lab_len == 0] <- "` `"
          lab <- sapply(X=lab, FUN=str2expression)
        } else {
          lab <- lab_expressions
        }
      }
      label_grob <- textAlongGrob(
        lab, label_data$x, label_data$y,
        default.units = 'native',
        hjust = label_data$hjust, vjust = label_data$vjust,
        rot = label_data$angle,
        gp = gpar(
          col = alpha(label_data$label_colour, label_data$label_alpha),
          fontsize = label_data$label_size * .pt,
          fontfamily = label_data$family,
          fontface = label_data$fontface,
          lineheight = label_data$lineheight
        ),
        check.overlap = check_overlap, rot.type = angle_calc,
        x0 = label_x0, y0 = label_y0, x1 = label_x1, y1 = label_y1,
        force.rot = force_flip, dodge = label_dodge, push = label_push
      )
      gList(edge_grob, label_grob)
    } else {
      edge_grob
    }
  },
  draw_key = function(data, params, size) {
    segmentsGrob(0.1, 0.5, 0.9, 0.5,
      gp = gpar(
        col = alpha(data$edge_colour, data$edge_alpha),
        lwd = data$edge_width * .pt,
        lty = data$edge_linetype, lineend = 'butt'
      ),
      arrow = params$arrow
    )
  },
  handle_na = function(data, params) {
    if (params$interpolate) {
      return(data)
    }
    keep <- function(x) {
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first),
        rep(TRUE, last - first),
        rep(FALSE, length(x) - last)
      )
    }
    missing <- !stats::complete.cases(
      data[c('x', 'y', 'edge_width', 'edge_colour', 'edge_linetype')]
    )
    kept <- stats::ave(missing, data$group, FUN = keep)
    data <- data[kept, ]
    if (!all(kept) && !params$na.rm) {
      warning('Removed ', sum(!kept), ' rows containing missing values',
        ' (geom_edge_path).',
        call. = FALSE
      )
    }
    data
  },
  default_aes = aes(
    edge_colour = 'black', edge_width = 0.5, edge_linetype = 'solid',
    edge_alpha = NA, start_cap = NA, end_cap = NA, label = NA,
    label_pos = 0.5, label_size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5, family = '', fontface = 1,
    lineheight = 1.2
  )
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid gpar segmentsGrob
#' @export
GeomEdgeParallelPath <- ggproto('GeomEdgeParallelPath', GeomEdgePath,
  draw_panel = function(data, panel_scales, coord, arrow = NULL, sep = unit(2, 'mm'),
                        lineend = 'butt', linejoin = 'round', linemitre = 1,
                        na.rm = FALSE, interpolate = TRUE,
                        label_colour = 'black', label_alpha = 1, label_parse = FALSE,
                        check_overlap = FALSE, angle_calc = 'none', force_flip = TRUE,
                        label_dodge = NULL, label_push = NULL) {
    data <- data[order(data$group), , drop = FALSE]
    panel <- GeomEdgePath$draw_panel(data, panel_scales, coord, arrow = arrow,
                                     lineend = lineend, linejoin = linejoin,
                                     linemitre = linemitre, na.rm = na.rm,
                                     interpolate = interpolate,
                                     label_colour = label_colour,
                                     label_alpha = label_alpha,
                                     label_parse = label_parse,
                                     check_overlap = check_overlap,
                                     angle_calc = angle_calc,
                                     force_flip = force_flip,
                                     label_dodge = label_dodge,
                                     label_push = label_push)
    if (inherits(panel, 'gList')) {
      panel[[1]]$sep <- (data$.position * sep)[panel[[1]]$id]
      class(panel[[1]]) <- c('parallelPath', class(panel[[1]]))
    } else {
      panel$sep <- (data$.position[!duplicated(data$group)] * sep)[panel$id]
      class(panel) <- c('parallelPath', class(panel))
    }
    panel
  }
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid gpar segmentsGrob
#' @export
GeomEdgeSegment <- ggproto('GeomEdgeSegment', GeomSegment,
  draw_panel = function(data, panel_scales, coord, arrow = NULL, lineend = 'butt',
                          na.rm = FALSE) {
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(zeroGrob())
    }
    coord <- coord$transform(data, panel_scales)
    segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
      default.units = 'native',
      gp = gpar(
        col = alpha(coord$edge_colour, coord$edge_alpha),
        fill = alpha(coord$edge_colour, coord$edge_alpha),
        lwd = coord$edge_width * .pt,
        lty = coord$edge_linetype,
        lineend = lineend
      ),
      arrow = arrow
    )
  },
  draw_key = function(data, params, size) {
    segmentsGrob(0.1, 0.5, 0.9, 0.5,
      gp = gpar(
        col = alpha(data$edge_colour, data$edge_alpha),
        lwd = data$edge_width * .pt,
        lty = data$edge_linetype, lineend = 'butt'
      ),
      arrow = params$arrow
    )
  },
  default_aes = aes(
    edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
    edge_alpha = NA
  )
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomEdgeParallelSegment <- ggproto('GeomEdgeParallelSegment', GeomEdgeSegment,
  draw_panel = function(data, panel_scales, coord, arrow = NULL, lineend = 'butt',
                        na.rm = FALSE, sep = unit(2, 'mm')) {
    data <- data[order(data$group), , drop = FALSE]
    panel <- GeomEdgeSegment$draw_panel(data, panel_scales, coord,
                                        arrow = arrow, lineend = lineend,
                                        na.rm = na.rm)
    panel$sep <- data$.position * sep
    class(panel) <- c('parallelPath', class(panel))
    panel
  }
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid pointsGrob unit gpar grobTree
#' @export
GeomEdgeSpanPath <- ggproto('GeomEdgeSpanPath', GeomEdgePath,
  draw_panel = function(data, panel_scales, coord, arrow = NULL, end_shape = NA,
                        lineend = 'butt', linejoin = 'round', linemitre = 1,
                        na.rm = FALSE, interpolate = TRUE,
                        label_colour = 'black', label_alpha = 1, label_parse = FALSE,
                        check_overlap = FALSE, angle_calc = 'none', force_flip = TRUE,
                        label_dodge = NULL, label_push = NULL) {
    panel <- GeomEdgePath$draw_panel(data, panel_scales, coord, arrow = arrow,
                                     lineend = lineend, linejoin = linejoin, linemitre = linemitre,
                                     na.rm = na.rm, interpolate = interpolate,
                                     label_colour = label_colour, label_alpha = label_alpha, label_parse = label_parse,
                                     check_overlap = FALSE, angle_calc = 'none', force_flip = TRUE,
                                     label_dodge = label_dodge, label_push = label_push)
    if (is.na(end_shape)) return(panel)

    data <- data[data$index == 0 | data$index == 1, ]
    end_shape <- translate_pch(end_shape)
    coords <- coord$transform(data, panel_scales)
    ends <- pointsGrob(coords$x, coords$y,
      pch = end_shape,
      size = unit(1 / (panel_scales$x.range[2] - (panel_scales$x.range[1])), 'npc'),
      gp = gpar(
        col = alpha(coords$edge_colour, coords$edge_alpha),
        fill = alpha(coords$edge_colour, coords$edge_alpha),
        lwd = 0
      )
    )
    grobTree(panel, ends)
  }
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid pointsGrob unit gpar grobTree
#' @export
GeomEdgeSpanSegment <- ggproto('GeomEdgeSpanSegment', GeomEdgeSegment,
  draw_panel = function(data, panel_scales, coord, arrow = NULL, lineend = 'butt',
                        na.rm = FALSE, end_shape = NA) {
    panel <- GeomEdgeSegment$draw_panel(data, panel_scales, coord, arrow = arrow, lineend = lineend,
                                        na.rm = na.rm)
    if (is.na(end_shape)) return(panel)

    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    data <- rbind_dfs(list(data, data2))
    data$xend <- NULL
    data$yend <- NULL

    end_shape <- translate_pch(end_shape)
    coords <- coord$transform(data, panel_scales)
    ends <- pointsGrob(coords$x, coords$y,
      pch = end_shape,
      size = unit(1 / (panel_scales$x.range[2] - (panel_scales$x.range[1])), 'npc'),
      gp = gpar(
        col = alpha(coords$edge_colour, coords$edge_alpha),
        fill = alpha(coords$edge_colour, coords$edge_alpha),
        lwd = 0
      )
    )
    grobTree(panel, ends)
  }
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid gpar pointsGrob
#' @export
GeomEdgePoint <- ggproto('GeomEdgePoint', GeomPoint,
  setup_data = function(data, params) {
    if (params$mirror) {
      data2 <- data
      data2[, c('x', 'y')] <- data2[, c('y', 'x'), drop = FALSE]
      data2$x <- abs(data2$x) * sign(data$x)
      data2$y <- abs(data2$y) * sign(data$y)
      data <- rbind_dfs(list(data, data2))
    }
    data
  },
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(zeroGrob())
    }
    data$edge_shape <- translate_pch(data$edge_shape)
    coords <- coord$transform(data, panel_scales)
    coords <- coords[order(coords$edge_size, decreasing = TRUE), , drop = FALSE]
    pointsGrob(coords$x, coords$y,
      pch = coords$edge_shape,
      gp = gpar(
        col = alpha(coords$edge_colour, coords$edge_alpha),
        fill = alpha(coords$edge_fill, coords$edge_alpha),
        fontsize = coords$edge_size * .pt + coords$stroke *
          .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )
  },
  draw_key = function(data, params, size) {
    pointsGrob(0.5, 0.5,
      pch = data$edge_shape,
      gp = gpar(
        col = alpha(data$edge_colour, data$edge_alpha),
        fill = alpha(data$edge_fill, data$edge_alpha),
        fontsize = data$edge_size * .pt + data$stroke * .stroke / 2,
        lwd = data$stroke * .stroke / 2
      )
    )
  },
  default_aes = aes(
    edge_shape = 19, edge_colour = 'black', edge_size = 1.5,
    edge_fill = NA, edge_alpha = NA, stroke = 0.5
  ),
  extra_params = c('na.rm', 'mirror')
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid gpar rectGrob
#' @export
GeomEdgeTile <- ggproto('GeomEdgeTile', GeomTile,
  setup_data = function(data, params) {
    if (params$mirror) {
      data2 <- data
      data2[, c('x', 'y')] <- data2[, c('y', 'x'), drop = FALSE]
      data2$x <- abs(data2$x) * sign(data$x)
      data2$y <- abs(data2$y) * sign(data$y)
      data <- rbind_dfs(list(data, data2))
    }
    data
  },
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(zeroGrob())
    }
    data$xmin <- data$x - 0.5
    data$xmax <- data$x + 0.5
    data$ymin <- data$y - 0.5
    data$ymax <- data$y + 0.5
    coords <- coord$transform(data, panel_params)
    rectGrob(
      coords$xmin, coords$ymax,
      width = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      default.units = "native",
      just = 'centre',
      gp = gpar(
        col = coords$edge_colour,
        fill = alpha(coords$edge_fill, coords$edge_alpha),
        lwd = coords$edge_size * .pt,
        lty = coords$edge_linetype,
        linejoin = 'mitre',
        # `lineend` is a workaround for Windows and intentionally kept unexposed
        # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
        lineend = "square"
      )
    )
  },
  draw_key = function(data, params, size) {
    if (is.null(data$edge_size)) {
      data$edge_size <- 0.5
    }

    lwd <- min(data$edge_size, min(size) / 4)

    rectGrob(
      width = unit(1, "npc") - unit(lwd, "mm"),
      height = unit(1, "npc") - unit(lwd, "mm"),
      gp = gpar(
        col = data$edge_colour %||% NA,
        fill = alpha(data$edge_fill %||% "grey20", data$edge_alpha),
        lty = data$edge_linetype %||% 1,
        lwd = lwd * .pt,
        linejoin = "mitre",
        # `lineend` is a workaround for Windows and intentionally kept unexposed
        # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
        lineend = "square"
      ))
  },
  default_aes = aes(
    edge_fill = 'grey20', edge_colour = NA, edge_size = 0.1,
    edge_linetype = 1, edge_alpha = NA
  ),
  extra_params = c('na.rm', 'mirror')
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce GeomBezier0
#' @export
GeomEdgeBezier <- ggproto('GeomEdgeBezier', GeomBezier0,
  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = 'butt', linejoin = 'round', linemitre = 1,
                          na.rm = FALSE) {
    names(data) <- sub('edge_', '', names(data))
    names(data)[names(data) == 'width'] <- 'size'
    GeomBezier0$draw_panel(data, panel_scales, coord, arrow, lineend, linejoin, linemitre, na.rm)
  },
  draw_key = function(data, params, size) {
    segmentsGrob(0.1, 0.5, 0.9, 0.5,
      gp = gpar(
        col = alpha(data$edge_colour, data$edge_alpha),
        lwd = data$edge_width * .pt,
        lty = data$edge_linetype, lineend = 'butt'
      ),
      arrow = params$arrow
    )
  },
  default_aes = aes(
    edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
    edge_alpha = NA
  ),
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
                          lineend = 'butt', linejoin = 'round', linemitre = 1,
                          na.rm = FALSE) {
    names(data) <- sub('edge_', '', names(data))
    names(data)[names(data) == 'width'] <- 'size'
    GeomBspline0$draw_panel(data, panel_scales, coord, arrow, lineend, linejoin, linemitre, na.rm)
  },
  draw_key = function(data, params, size) {
    segmentsGrob(0.1, 0.5, 0.9, 0.5,
      gp = gpar(
        col = alpha(data$edge_colour, data$edge_alpha),
        lwd = data$edge_width * .pt,
        lty = data$edge_linetype, lineend = 'butt'
      ),
      arrow = params$arrow
    )
  },
  default_aes = aes(
    edge_colour = 'black', edge_width = 0.5, edge_linetype = 1,
    edge_alpha = NA
  ),
  handle_na = function(data, ...) {
    data
  }
)

remove_loop <- function(data) {
  if (nrow(data) == 0) return(data)

  data[!(data$x == data$xend & data$y == data$yend), , drop = FALSE]
}
remove_loop2 <- function(data) {
  if (nrow(data) == 0) return(data)

  data <- data[order(data$group), ]
  loop <- data$x[c(TRUE, FALSE)] == data$x[c(FALSE, TRUE)] &
    data$y[c(TRUE, FALSE)] == data$y[c(FALSE, TRUE)]
  data[rep(!loop, each = 2), , drop = FALSE]
}
