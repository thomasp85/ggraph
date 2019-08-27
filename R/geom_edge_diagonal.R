#' Draw edges as diagonals
#'
#' This geom draws edges as diagonal bezier curves. The name comes from D3.js
#' where this shape was called diagonals until it was renamed to
#' [links](https://github.com/d3/d3-shape/blob/v1.3.5/README.md#links).
#' A diagonal in this context is a quadratic bezier with the control points
#' positioned halfway between the start and end points but on the same axis.
#' This produces a pleasing fan-in, fan-out line that is mostly relevant for
#' hierarchical layouts as it implies an overall directionality in the plot.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_diagonal` and `geom_edge_diagonal0` understand the following
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
#' `geom_edge_diagonal2` understand the following aesthetics. Bold aesthetics are
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
#' `geom_edge_diagonal` and `geom_edge_diagonal2` furthermore takes the following
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
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param flipped Logical, Has the layout been flipped by reassigning the
#' mapping of x, y etc?
#'
#' @param strength The strength of the curvature of the diagonal. `0` will
#' result in a straight line while `1` will give the familiar S-shape.
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
#'   geom_edge_diagonal(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'tree') +
#'   geom_edge_diagonal2(aes(colour = node.class))
#'
#' ggraph(gr, 'tree') +
#'   geom_edge_diagonal0(aes(colour = class))
#' @rdname geom_edge_diagonal
#' @name geom_edge_diagonal
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBezier
#' @export
StatEdgeDiagonal <- ggproto('StatEdgeDiagonal', StatBezier,
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
    create_diagonal(data, data2, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'n', 'strength')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal <- function(mapping = NULL, data = get_edges(),
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
    data = data, mapping = mapping, stat = StatEdgeDiagonal,
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
StatEdgeDiagonal2 <- ggproto('StatEdgeDiagonal2', StatBezier2,
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
    create_diagonal(data, data2, params)
  },
  required_aes = c('x', 'y', 'group', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'n', 'strength')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal2 <- function(mapping = NULL, data = get_edges('long'),
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
    data = data, mapping = mapping, stat = StatEdgeDiagonal2,
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
StatEdgeDiagonal0 <- ggproto('StatEdgeDiagonal0', StatBezier0,
  setup_data = function(data, params) {
    StatEdgeDiagonal$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'circular'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'strength')
)
#' @rdname geom_edge_diagonal
#'
#' @export
geom_edge_diagonal0 <- function(mapping = NULL, data = get_edges(),
                                position = 'identity', arrow = NULL, strength = 1,
                                flipped = FALSE, lineend = 'butt',
                                show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeDiagonal0,
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

create_diagonal <- function(from, to, params) {
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
    r_mod <- params$strength * (r1 - r0) / 2

    data2$x[from$circular] <- from$x[from$circular] / (r0 / (r0 + r_mod))
    data2$y[from$circular] <- from$y[from$circular] / (r0 / (r0 + r_mod))
    data3$x[from$circular] <- to$x[from$circular] / (r1 / (r1 - r_mod))
    data3$y[from$circular] <- to$y[from$circular] / (r1 / (r1 - r_mod))

    data2$x[root] <- from$x[root]
    data2$y[root] <- from$y[root]
    data3$x[root] <- to$x[root]
    data3$y[root] <- to$y[root]
  }
  if (any(!from$circular)) {
    if (params$flipped) {
      h_diff <- from$x[!from$circular] - to$x[!from$circular]
      data2$x[!from$circular] <- from$x[!from$circular] - h_diff / 2 * params$strength
      data3$x[!from$circular] <- to$x[!from$circular] + h_diff / 2 * params$strength
    } else {
      h_diff <- from$y[!from$circular] - to$y[!from$circular]
      data2$y[!from$circular] <- from$y[!from$circular] - h_diff / 2 * params$strength
      data3$y[!from$circular] <- to$y[!from$circular] + h_diff / 2 * params$strength
    }
  }
  data <- rbind_dfs(list(from, data2, data3, to))
  data[order(data$index), names(data) != 'index']
}
