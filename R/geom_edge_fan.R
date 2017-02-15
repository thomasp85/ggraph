#' Draw edges as curves of different curvature
#'
#' This geom draws edges as cubic beziers with the control point positioned
#' half-way between the nodes and at an angle dependent on the presence of
#' parallel edges. This results in parallel edges being drawn in a
#' non-overlapping fashion resembling the standard approach used in
#' \code{\link[igraph]{plot.igraph}}. Before calculating the curvature the edges
#' are sorted by direction so that edges goind the same way will be adjacent.
#' This geom is currently the only choice for non-simple graphs if edges should
#' not be overplotted.
#'
#' @details
#' Many geom_edge_* layers comes in 3 flavors depending on the level of control
#' needed over the drawing. The default (no numeric postfix) generate a number
#' of points (\code{n}) along the edge and draws it as a path. Each point along
#' the line has a numeric value associated with it giving the position along the
#' path, and it is therefore possible to show the direction of the edge by
#' mapping to this e.g. \code{colour = ..index..}. The version postfixed with a
#' "2" uses the "long" edge format (see \code{\link{get_edges}}) and makes it
#' possible to interpolate node parameter between the start and end node along
#' the edge. It is considerable less performant so should only be used if this
#' is needed. The version postfixed with a "0" draws the edge in the most
#' performant way, often directly using an appropriate grob from the grid
#' package, but does not allow for gradients along the edge.
#'
#' Often it is beneficial to stop the drawing of the edge before it reaches the
#' node, for instance in cases where an arrow should be drawn and the arrowhead
#' shouldn't lay ontop or below the node point. geom_edge_* and geom_edge_*2
#' supports this through the start_cap and end_cap aesthetics that takes a
#' \code{\link{geometry}} specification and dynamically caps the termini of the
#' edges based on the given specifications. This means that if
#' \code{end_cap = circle(1, 'cm')} the edges will end at a distance of 1cm even
#' during resizing of the plot window.
#'
#' All \code{geom_edge_*} and \code{geom_edge_*2} have the ability to draw a
#' label along the edge. The reason this is not a separate geom is that in order
#' for the label to know the location of the edge it needs to know the edge type
#' etc. Labels are drawn by providing a label aesthetic. The label_pos can be
#' used to specify where along the edge it should be drawn by supplying a number
#' between 0 and 1. The label_size aesthetic can be used to control the size of
#' the label. Often it is needed to have the label written along the direction
#' of the edge, but since the actual angle is dependent on the plot dimensions
#' this cannot be calculated beforehand. Using the angle_calc argument allows
#' you to specify whether to use the supplied angle aesthetic or whether to draw
#' the label along or across the edge.
#'
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' \code{edge_colour} within the \code{aes()} call as \code{colour} will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_edge_fan and geom_edge_fan0 understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{xend}}
#'  \item{\strong{yend}}
#'  \item{\strong{from}}
#'  \item{\strong{to}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_fan2 understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{group}}
#'  \item{\strong{from}}
#'  \item{\strong{to}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#' geom_edge_fan and geom_edge_fan2 furthermore takes the following
#' aesthetics.
#' \itemize{
#'   \item{start_cap}
#'   \item{end_cap}
#'   \item{label}
#'   \item{label_pos}
#'   \item{label_size}
#'   \item{angle}
#'   \item{hjust}
#'   \item{vjust}
#'   \item{family}
#'   \item{fontface}
#'   \item{lineheight}
#' }
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param spread Modify the width of the fans \code{spread > 1} will create
#' wider fans while the reverse will make them more narrow.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(igraph)
#' gr <- graph_from_data_frame(data.frame(
#'   from = c(1, 1, 1, 1, 1, 2, 2, 2),
#'   to = c(2, 2, 2, 2, 2, 1, 1, 1),
#'   class = sample(letters[1:3], 8, TRUE)
#' ))
#' V(gr)$class <- c('a', 'b')
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_fan(aes(alpha = ..index..))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_fan2(aes(colour = node.class))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_fan0(aes(colour = class))
#'
#' @rdname geom_edge_fan
#' @name geom_edge_fan
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
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
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan <- function(mapping = NULL, data = get_edges(),
                          position = "identity", arrow = NULL,
                          spread = 1, n = 100, lineend = "butt",
                          linejoin = "round", linemitre = 1,
                          label_colour = 'black',  label_alpha = 1,
                          label_parse = FALSE, check_overlap = FALSE,
                          angle_calc = 'rot', force_flip = TRUE,
                          label_dodge = NULL, label_push = NULL,
                          show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          from=~from, to=~to))
    layer(data = data, mapping = mapping, stat = StatEdgeFan,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = FALSE, spread = spread,
                   label_colour = label_colour, label_alpha = label_alpha,
                   label_parse = label_parse, check_overlap = check_overlap,
                   angle_calc = angle_calc, force_flip = force_flip,
                   label_dodge = label_dodge, label_push = label_push, ...)
          )
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
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
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan2 <- function(mapping = NULL, data = get_edges('long'),
                           position = "identity", arrow = NULL,
                           spread = 1, n = 100, lineend = "butt",
                           linejoin = "round", linemitre = 1,
                           label_colour = 'black',  label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge.id,
                                          from=~from, to=~to))
    layer(data = data, mapping = mapping, stat = StatEdgeFan2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, na.rm = FALSE, n = n,
                   interpolate = TRUE, spread = spread,
                   label_colour = label_colour, label_alpha = label_alpha,
                   label_parse = label_parse, check_overlap = check_overlap,
                   angle_calc = angle_calc, force_flip = force_flip,
                   label_dodge = label_dodge, label_push = label_push, ...)
          )
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier0
#' @export
StatEdgeFan0 <- ggproto('StatEdgeFan0', StatBezier0,
    setup_data = function(data, params) {
        StatEdgeFan$setup_data(data, params)
    },
    required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan0 <- function(mapping = NULL, data = get_edges(),
                                position = "identity", arrow = NULL, spread = 1,
                                lineend = "butt", show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                          from=~from, to=~to))
    layer(data = data, mapping = mapping, stat = StatEdgeFan0,
          geom = GeomEdgeBezier, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = expand_edge_aes(
              list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                        spread = spread, ...)
          )
    )
}
#' @importFrom dplyr %>% group_by_ arrange_ mutate_ n ungroup transmute_
createFans <- function(from, to, params) {
    from$.id <- paste(pmin(from$from, to$to), pmax(from$from, to$to), sep = '-')
    from$.origInd <- seq_len(nrow(from))
    position <- from %>% group_by_(~PANEL, ~.id) %>%
        arrange_(~from) %>%
        mutate_(position = ~seq_len(n()) - 0.5 - n()/2) %>%
        mutate_(position = ~position * ifelse(from < to, 1, -1)) %>%
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
