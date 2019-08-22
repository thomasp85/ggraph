#' Draw edges as diagonals
#'
#' This geom draws edges as cubic bezier curves with the control points
#' positioned along the elbow edge. It has the appearance of a softened elbow
#' edge with the hard angle substituted by a tapered bend.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_bend` and `geom_edge_bend0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **circular**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_bend2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **circular**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_bend` and `geom_edge_bend2` furthermore takes the following
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
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_diagonal
#'
#' @param strength The strength of the curvature of the bend. `0` will
#' result in a straight line while `1` will give a strong arc.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_tree(20, 4) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'tree') +
#'   geom_edge_bend(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'tree') +
#'   geom_edge_bend2(aes(colour = node.class))
#'
#' ggraph(gr, 'tree') +
#'   geom_edge_bend0(aes(colour = class))
#' @rdname geom_edge_bend
#' @name geom_edge_bend
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeBend <- ggproto('StatEdgeBend', StatBezier,
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
    create_bend(data, data2, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'n', 'strength')
)
#' @rdname geom_edge_bend
#'
#' @export
geom_edge_bend <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL, strength = 1,
                           flipped = FALSE, n = 100, lineend = 'butt',
                           linejoin = 'round', linemitre = 1,
                           label_colour = 'black', label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular, group = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBend,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, flipped = flipped, strength = strength,
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
StatEdgeBend2 <- ggproto('StatEdgeBend2', StatBezier2,
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
    create_bend(data, data2, params)
  },
  required_aes = c('x', 'y', 'group', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'n', 'strength')
)
#' @rdname geom_edge_bend
#'
#' @export
geom_edge_bend2 <- function(mapping = NULL, data = get_edges('long'),
                            position = 'identity', arrow = NULL, strength = 1,
                            flipped = FALSE, n = 100, lineend = 'butt',
                            linejoin = 'round', linemitre = 1,
                            label_colour = 'black', label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id,
    circular = circular
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBend2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, flipped = flipped, strength = strength,
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
StatEdgeBend0 <- ggproto('StatEdgeBend0', StatBezier0,
  setup_data = function(data, params) {
    StatEdgeBend$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'strength')
)
#' @rdname geom_edge_bend
#'
#' @export
geom_edge_bend0 <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', arrow = NULL, strength = 1,
                            flipped = FALSE, lineend = 'butt',
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBend0,
    geom = GeomEdgeBezier, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE, strength = strength,
        flipped = flipped, ...
      )
    )
  )
}

create_bend <- function(from, to, params) {
  bezier_start <- seq(1, by = 4, length.out = nrow(from))
  from$index <- bezier_start
  to$index <- bezier_start + 3
  data2 <- from
  data3 <- to
  data2$index <- bezier_start + 1
  data3$index <- bezier_start + 2
  if (any(from$circular)) {
    r0 <- sqrt(from$x[from$circular]^2 + from$y[from$circular]^2)
    r1 <- sqrt(to$x[to$circular]^2 + to$y[to$circular]^2)
    root <- r0 == 0 | r1 == 0

    dotprod <- from$x * to$x + from$y * to$y

    crossing <- r0 * r0 / dotprod
    cross_x <- to$x * crossing
    cross_y <- to$y * crossing

    data2$x[from$circular] <- from$x + (cross_x - from$x) * params$strength
    data2$y[from$circular] <- from$y + (cross_y - from$y) * params$strength
    data3$x[from$circular] <- to$x + (cross_x - to$x) * params$strength
    data3$y[from$circular] <- to$y + (cross_y - to$y) * params$strength

    data2$x[root] <- from$x[root]
    data2$y[root] <- from$y[root]
    data3$x[root] <- to$x[root]
    data3$y[root] <- to$y[root]
  }
  if (any(!from$circular)) {
    if (params$flipped) {
      h_diff <- from$x[!from$circular] - to$x[!from$circular]
      w_diff <- from$y[!from$circular] - to$y[!from$circular]
      data2$y[!from$circular] <- from$y[!from$circular] - w_diff * params$strength
      data3$x[!from$circular] <- to$x[!from$circular] + h_diff * params$strength
    } else {
      h_diff <- from$y[!from$circular] - to$y[!from$circular]
      w_diff <- from$x[!from$circular] - to$x[!from$circular]
      data2$x[!from$circular] <- from$x[!from$circular] - w_diff * params$strength
      data3$y[!from$circular] <- to$y[!from$circular] + h_diff * params$strength
    }
  }
  data <- rbind_dfs(list(from, data2, data3, to))
  data[order(data$index), names(data) != 'index']
}
