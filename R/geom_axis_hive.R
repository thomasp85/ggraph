#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom dplyr %>% group_by_ mutate_ slice ungroup
#' @export
StatAxisHive <- ggproto('StatAxisHive', StatFilter,
    setup_data = function(data, params) {
        data <- data %>% group_by_(~angle, ~section, ~PANEL) %>%
            mutate_(x = ~min(r)*cos(angle[1]) * 1.1,
                    y = ~min(r)*sin(angle[1]) * 1.1,
                    xend = ~max(r)*cos(angle[1]) * 1.1,
                    yend = ~max(r)*sin(angle[1]) * 1.1,
                    max_r = ~max(r),
                    min_r = ~min(r)
            ) %>%
            slice(1) %>%
            ungroup()
        as.data.frame(data)
    },
    required_aes = c('r', 'angle', 'centerSize', 'axis', 'section'),
    extra_params = c('na.rm', 'n', 'curvature')
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid textGrob nullGrob
#' @importFrom dplyr %>% group_by_ summarise_
#' @export
GeomAxisHive <- ggproto('GeomAxisHive', GeomSegment,
    draw_panel = function(data, panel_scales, coord, label = TRUE, axis = TRUE, label_colour = 'black') {
        data$x <- data$x / 1.1
        data$y <- data$y / 1.1
        data$xend <- data$xend / 1.1
        data$yend <- data$yend / 1.1
        data <- coord$transform(data, panel_scales)
        labelData <- data %>% group_by_(~axis) %>%
            summarise_(x = ~max(max_r) * cos(mean(angle)),
                       y = ~max(max_r) * sin(mean(angle)),
                       label = ~axis[1],
                       angle = ~mean(angle)/(2*pi) * 360 - 90,
                       colour = ~colour[1],
                       label_size = ~label_size[1],
                       family = ~family[1],
                       fontface = ~fontface[1],
                       lineheight = ~lineheight[1]
            )
        labelData <- as.data.frame(labelData)
        labDist <- sqrt(labelData$x^2 + labelData$y^2)
        distDodge <- max(labDist) * 1.05 - max(labDist)
        labelData$x <- labelData$x * (distDodge + labDist)/labDist
        labelData$y <- labelData$y * (distDodge + labDist)/labDist
        labelData$angle <- labelData$angle + ifelse(labelData$angle < 0, 360, 0)
        labelData$angle <- labelData$angle - ifelse(labelData$angle > 360, 360, 0)
        upsideLabel <- labelData$angle > 90 & labelData$angle < 270
        labelData$angle[upsideLabel] <- labelData$angle[upsideLabel] + 180
        labelData <- coord$transform(labelData, panel_scales)
        labelData$label_colour <- if (is.na(label_colour)) {
            labelData$colour
        } else {
            label_colour
        }
        labelGrob <- if (label) {
            textGrob(labelData$label, labelData$x, labelData$y,
                     default.units = 'native', rot = labelData$angle,
                     gp = gpar(col = labelData$label_colour,
                               fontsize = labelData$label_size * .pt,
                               fontfamily = labelData$family,
                               fontface = labelData$fontface,
                               lineheight = labelData$lineheight))
        } else {
            nullGrob()
        }
        axisGrob <- if (axis) {
            segmentsGrob(data$x, data$y, data$xend, data$yend,
                         default.units = 'native',
                         gp = gpar(col = alpha(data$colour, data$alpha),
                                   fill = alpha(data$colour, data$alpha),
                                   lwd = data$size * .pt,
                                   lty = data$linetype,
                                   lineend = 'square')
            )
        } else {
            nullGrob()
        }
        gList(axisGrob, labelGrob)
    },
    default_aes = aes(colour = 'black', size = 0.5, linetype = 1, alpha = NA,
                      label_size = 3.88, family = '', fontface = 1,
                      lineheight = 1.2)
)

#' Draw rectangular bars and labels on hive axes
#'
#' This function lets you annotate the axes in a hive plot with labels and
#' color coded bars.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param label Should the axes be labelled. Defaults to \code{TRUE}
#'
#' @param axis Should a rectangle be drawn along the axis. Defaults to \code{TRUE}
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
#' library(igraph)
#' flareGr <- graph_from_data_frame(flare$imports)
#' # Add some metadata to divide nodes by
#' V(flareGr)$type <- 'Both'
#' V(flareGr)$type[degree(flareGr, mode = 'in') == 0] <- 'Source'
#' V(flareGr)$type[degree(flareGr, mode = 'out') == 0] <- 'Sink'
#' analyticsNodes <- grep('flare.analytics', V(flareGr)$name)
#' E(flareGr)$type <- 'Other'
#' E(flareGr)[inc(analyticsNodes)]$type <- 'Analytics'
#' ggraph(flareGr, 'hive', axis = 'type') +
#'     geom_edge_hive(aes(colour = type), edge_alpha = 0.1) +
#'     geom_axis_hive(aes(colour = type)) +
#'     coord_fixed()
#'
geom_axis_hive <- function(mapping = NULL, data = NULL,
                           position = "identity", label = TRUE, axis = TRUE, show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(r=~r, angle=~angle, centerSize=~centerSize, axis=~axis, section=~section))
    layer(data = data, mapping = mapping, stat = StatAxisHive,
          geom = GeomAxisHive, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(na.rm = FALSE, label = label, axis = axis, ...)
    )
}
