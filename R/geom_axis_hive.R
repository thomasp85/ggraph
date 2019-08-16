#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom dplyr %>% group_by mutate slice ungroup
#' @export
StatAxisHive <- ggproto('StatAxisHive', StatFilter,
  setup_data = function(data, params) {
    data <- data %>%
      group_by(.data$angle, .data$section, .data$PANEL) %>%
      mutate(
        x = min(.data$r) * cos(.data$angle[1]) * 1.1,
        y = min(.data$r) * sin(.data$angle[1]) * 1.1,
        xend = max(.data$r) * cos(.data$angle[1]) * 1.1,
        yend = max(.data$r) * sin(.data$angle[1]) * 1.1,
        max_r = max(.data$r),
        min_r = min(.data$r)
      ) %>%
      slice(1) %>%
      ungroup()
    as.data.frame(data, stringsAsFactors = FALSE)
  },
  required_aes = c('r', 'angle', 'center_size', 'axis', 'section'),
  extra_params = c('na.rm', 'n', 'curvature')
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid textGrob nullGrob
#' @importFrom dplyr %>% group_by summarise
#' @export
GeomAxisHive <- ggproto('GeomAxisHive', GeomSegment,
  draw_panel = function(data, panel_scales, coord, label = TRUE, axis = TRUE, label_colour = 'black') {
    data$x <- data$x / 1.1
    data$y <- data$y / 1.1
    data$xend <- data$xend / 1.1
    data$yend <- data$yend / 1.1
    data <- coord$transform(data, panel_scales)
    label_data <- data %>%
      group_by(.data$axis) %>%
      summarise(
        x = max(.data$max_r) * cos(mean(.data$angle)),
        y = max(.data$max_r) * sin(mean(.data$angle)),
        label = .data$axis[1],
        angle = mean(.data$angle) / (2 * pi) * 360 - 90,
        colour = .data$colour[1],
        label_size = .data$label_size[1],
        family = .data$family[1],
        fontface = .data$fontface[1],
        lineheight = .data$lineheight[1]
      )
    label_data <- as.data.frame(label_data, stringsAsFactors = FALSE)
    lab_dist <- sqrt(label_data$x^2 + label_data$y^2)
    dist_dodge <- max(lab_dist) * 1.05 - max(lab_dist)
    label_data$x <- label_data$x * (dist_dodge + lab_dist) / lab_dist
    label_data$y <- label_data$y * (dist_dodge + lab_dist) / lab_dist
    label_data$angle <- label_data$angle + ifelse(label_data$angle < 0, 360, 0)
    label_data$angle <- label_data$angle - ifelse(label_data$angle > 360, 360, 0)
    upside_label <- label_data$angle > 90 & label_data$angle < 270
    label_data$angle[upside_label] <- label_data$angle[upside_label] + 180
    label_data <- coord$transform(label_data, panel_scales)
    label_data$label_colour <- if (is.na(label_colour)) {
      label_data$colour
    } else {
      label_colour
    }
    label_grob <- if (label) {
      textGrob(label_data$label, label_data$x, label_data$y,
        default.units = 'native', rot = label_data$angle,
        gp = gpar(
          col = label_data$label_colour,
          fontsize = label_data$label_size * .pt,
          fontfamily = label_data$family,
          fontface = label_data$fontface,
          lineheight = label_data$lineheight
        )
      )
    } else {
      nullGrob()
    }
    axis_grob <- if (axis) {
      segmentsGrob(data$x, data$y, data$xend, data$yend,
        default.units = 'native',
        gp = gpar(
          col = alpha(data$colour, data$alpha),
          fill = alpha(data$colour, data$alpha),
          lwd = data$size * .pt,
          lty = data$linetype,
          lineend = 'square'
        )
      )
    } else {
      nullGrob()
    }
    gList(axis_grob, label_grob)
  },
  default_aes = aes(
    colour = 'black', size = 0.5, linetype = 1, alpha = NA,
    label_size = 3.88, family = '', fontface = 1,
    lineheight = 1.2
  )
)

#' Draw rectangular bars and labels on hive axes
#'
#' This function lets you annotate the axes in a hive plot with labels and
#' color coded bars.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param label Should the axes be labelled. Defaults to `TRUE`
#'
#' @param axis Should a rectangle be drawn along the axis. Defaults to `TRUE`
#'
#' @section Aesthetics:
#' geom_axis_hive understand the following aesthetics.
#' \itemize{
#'  \item{alpha}
#'  \item{colour}
#'  \item{fill}
#'  \item{size}
#'  \item{linetype}
#'  \item{label_size}
#'  \item{family}
#'  \item{fontface}
#'  \item{lineheight}
#' }
#'
#' @author Thomas Lin Pedersen
#'
#' @export
#'
#' @examples
#' # Plot the flare import graph as a hive plot
#' library(tidygraph)
#' flareGr <- as_tbl_graph(flare$imports) %>%
#'   mutate(
#'     type = dplyr::case_when(
#'       centrality_degree(mode = 'in') == 0 ~ 'Source',
#'       centrality_degree(mode = 'out') == 0 ~ 'Sink',
#'       TRUE ~ 'Both'
#'     )
#'   ) %>%
#'   activate(edges) %>%
#'   mutate(
#'     type = dplyr::case_when(
#'       grepl('flare.analytics', paste(.N()$name[from], .N()$name[to])) ~ 'Analytics',
#'       TRUE ~ 'Other'
#'     )
#'   )
#' ggraph(flareGr, 'hive', axis = type) +
#'   geom_edge_hive(aes(colour = type), edge_alpha = 0.1) +
#'   geom_axis_hive(aes(colour = type)) +
#'   coord_fixed()
geom_axis_hive <- function(mapping = NULL, data = NULL,
                           position = 'identity', label = TRUE, axis = TRUE, show.legend = NA, ...) {
  mapping <- aes_intersect(mapping, aes(r = r, angle = angle,
                                        center_size = center_size,
                                        axis = axis, section = section))
  layer(
    data = data, mapping = mapping, stat = StatAxisHive,
    geom = GeomAxisHive, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(na.rm = FALSE, label = label, axis = axis, ...)
  )
}
