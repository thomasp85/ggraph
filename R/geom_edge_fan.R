#' Draw edges as curves of different curvature
#'
#' This geom draws edges as cubic beziers with the control point positioned
#' half-way between the nodes and at an angle dependent on the presence of
#' parallel edges. This results in parallel edges being drawn in a
#' non-overlapping fashion resembling the standard approach used in
#' [igraph::plot.igraph()]. Before calculating the curvature the edges
#' are sorted by direction so that edges going the same way will be adjacent.
#' This geom is currently the only choice for non-simple graphs if edges should
#' not be overplotted.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_fan` and `geom_edge_fan0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **from**
#' - **to**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_fan2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **from**
#' - **to**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_fan` and `geom_edge_fan2` furthermore takes the following
#' aesthetics.
#'
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
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
#' @param strength Modify the width of the fans `strength > 1` will create
#' wider fans while the reverse will make them more narrow.
#'
#' @param spread Deprecated. Use `strength` instead.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   convert(to_directed) %>%
#'   bind_edges(data.frame(from = c(1, 2, 2, 3), to = c(2, 1, 3, 2))) %E>%
#'   mutate(class = sample(letters[1:3], 9, TRUE)) %N>%
#'   mutate(class = sample(c('x', 'y'), 5, TRUE))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_fan(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_fan2(aes(colour = node.class))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_fan0(aes(colour = class))
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
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    data$group <- make_unique(data$group)
    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    create_fans(data, data2, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'strength', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan <- function(mapping = NULL, data = get_edges(),
                          position = 'identity', arrow = NULL,
                          strength = 1, n = 100, lineend = 'butt',
                          linejoin = 'round', linemitre = 1,
                          label_colour = 'black', label_alpha = 1,
                          label_parse = FALSE, check_overlap = FALSE,
                          angle_calc = 'rot', force_flip = TRUE,
                          label_dodge = NULL, label_push = NULL,
                          show.legend = NA, ..., spread) {
  if (!missing(spread)) {
    .Deprecated(msg = 'The spread argument has been deprecated in favour of strength')
    strength <- spread
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    from = from, to = to, group = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeFan,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, strength = strength,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
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
    data <- remove_loop2(data)
    if (nrow(data) == 0) return(NULL)
    data <- data[order(data$group), ]
    data2 <- data[c(FALSE, TRUE), ]
    data <- data[c(TRUE, FALSE), ]
    create_fans(data, data2, params)
  },
  required_aes = c('x', 'y', 'group', 'from', 'to'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'strength', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan2 <- function(mapping = NULL, data = get_edges('long'),
                           position = 'identity', arrow = NULL,
                           strength = 1, n = 100, lineend = 'butt',
                           linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ..., spread) {
  if (!missing(spread)) {
    .Deprecated(msg = 'The spread argument has been deprecated in favour of strength')
    strength <- spread
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id,
    from = from, to = to
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeFan2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, strength = strength,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
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
  extra_params = c('na.rm', 'strength', 'spread')
)
#' @rdname geom_edge_fan
#'
#' @export
geom_edge_fan0 <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL, strength = 1,
                           lineend = 'butt', show.legend = NA, ..., spread) {
  if (!missing(spread)) {
    .Deprecated(msg = 'The spread argument has been deprecated in favour of strength')
    strength <- spread
  }
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    from = from, to = to
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeFan0,
    geom = GeomEdgeBezier, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        strength = strength, ...
      )
    )
  )
}
#' @importFrom dplyr %>% group_by arrange mutate n ungroup transmute
create_fans <- function(from, to, params) {
  from$.id <- paste(pmin(from$from, to$to), pmax(from$from, to$to), sep = '-')
  from$.orig_ind <- seq_len(nrow(from))
  position <- from %>%
    group_by(.data$PANEL, .data$.id) %>%
    arrange(.data$from) %>%
    mutate(position = seq_len(n()) - 0.5 - n() / 2) %>%
    mutate(position = .data$position * ifelse(.data$from < .data$to, 1, -1)) %>%
    ungroup() %>%
    arrange(.data$.orig_ind) %>%
    transmute(position = .data$position)
  position <- position$position
  max_fans <- max(table(from$.id))
  from$.id <- NULL
  from$.orig_ind <- NULL
  mean_x <- rowMeans(cbind(from$x, to$x))
  mean_y <- rowMeans(cbind(from$y, to$y))
  step_x <- -(params$strength * (to$y - from$y) / (2 * max_fans))
  step_y <- params$strength * (to$x - from$x) / (2 * max_fans)
  data <- from
  data$x <- mean_x + step_x * position
  data$y <- mean_y + step_y * position
  bezier_start <- seq(1, by = 3, length.out = nrow(from))
  from$index <- bezier_start
  to$index <- bezier_start + 2
  data$index <- bezier_start + 1
  data <- rbind_dfs(list(from, data, to))
  data[order(data$index), names(data) != 'index']
}
