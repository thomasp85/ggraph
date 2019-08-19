#' Draw multi edges as parallel lines
#'
#' This geom draws multi edges as parallel lines. The edges are first sorted by
#' direction and then shifted a fixed amount so that all edges are visible.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_parallel` and `geom_edge_parallel0` understand the following
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
#' `geom_edge_parallel2` understand the following aesthetics. Bold aesthetics are
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
#' `geom_edge_parallel` and `geom_edge_parallel2` furthermore takes the following
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
#' @param sep The separation between parallel edges, given as a [grid::unit()]
#'
#' @author David Schoch and Thomas Lin Pedersen
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
#'   geom_edge_parallel(aes(alpha = stat(index)))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_parallel2(aes(colour = node.class))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_parallel0(aes(colour = class))
#'
#' # Use capping and sep to fine tune the look
#' ggraph(gr, 'stress') +
#'   geom_edge_parallel(start_cap = circle(1), end_cap = circle(1),
#'                      arrow = arrow(length = unit(2, 'mm')), sep = unit(4, 'mm')) +
#'   geom_node_point(size = 12)
#'
#' @rdname geom_edge_parallel
#' @name geom_edge_parallel
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatLink
#' @export
StatEdgeParallel <- ggproto('StatEdgeParallel', StatLink,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    data$.position <- edge_positions(data, data2, params)
    StatLink$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm", "n")
)

#' @rdname geom_edge_parallel
#'
#' @importFrom ggforce StatLink
#' @export
geom_edge_parallel <- function(mapping = NULL, data = get_edges(),
                               position = "identity", arrow = NULL,
                               sep = unit(2, 'mm'), n = 100, lineend = "butt",
                               linejoin = "round", linemitre = 1,
                               label_colour = 'black',  label_alpha = 1,
                               label_parse = FALSE, check_overlap = FALSE,
                               angle_calc = 'rot', force_flip = TRUE,
                               label_dodge = NULL, label_push = NULL,
                               show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y, xend = xend, yend = yend,
                                        from = from, to = to, group = edge.id))
  layer(data = data, mapping = mapping, stat = StatEdgeParallel,
        geom = GeomEdgeParallelPath, position = position, show.legend = show.legend,
        inherit.aes = FALSE,
        params = expand_edge_aes(
          list(arrow = arrow, lineend = lineend, linejoin = linejoin,
               linemitre = linemitre, na.rm = FALSE, sep = sep, n = n,
               interpolate = FALSE,
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
StatEdgeParallel2 <- ggproto('StatEdgeParallel2', StatLink2,
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
    data1 <- data[c(TRUE, FALSE), ]
    data$.position <- rep(edge_positions(data1, data2), each = 2)
    StatLink2$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'group', 'from', 'to'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n')
)
#' @rdname geom_edge_parallel
#'
#' @export
geom_edge_parallel2 <- function(mapping = NULL, data = get_edges('long'),
                                position = 'identity', arrow = NULL,
                                sep = unit(2, 'mm'), n = 100, lineend = 'butt',
                                linejoin = 'round', linemitre = 1,
                                label_colour = 'black', label_alpha = 1,
                                label_parse = FALSE, check_overlap = FALSE,
                                angle_calc = 'rot', force_flip = TRUE,
                                label_dodge = NULL, label_push = NULL,
                                show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id,
    from = from, to = to
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeParallel2,
    geom = GeomEdgeParallelPath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(arrow = arrow, lineend = lineend, linejoin = linejoin,
           linemitre = linemitre, na.rm = FALSE, sep = sep, n = n,
           interpolate = TRUE,
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
StatEdgeParallel0 <- ggproto('StatEdgeFan0', StatIdentity,
  setup_data = function(data, params) {
    StatEdgeParallel$setup_data(data, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'from', 'to'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm')
)
#' @rdname geom_edge_parallel
#'
#' @export
geom_edge_parallel0 <- function(mapping = NULL, data = get_edges(),
                           position = 'identity', arrow = NULL, sep = unit(2, 'mm'),
                           lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    from = from, to = to
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeParallel0,
    geom = GeomEdgeParallelSegment, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        sep = sep, ...
      )
    )
  )
}
#' @importFrom dplyr %>% group_by arrange summarise n ungroup pull
edge_positions <- function(from, to, params) {
  from$.id <- paste(pmin(from$from, to$to), pmax(from$from, to$to), sep = '-')
  from$.orig_ind <- seq_len(nrow(from))
  from %>%
    group_by(.data$PANEL, .data$.id) %>%
    arrange(.data$from) %>%
    mutate(position = seq_len(n()) - 0.5 - n() / 2) %>%
    mutate(position = .data$position * ifelse(.data$from < .data$to, 1, -1)) %>%
    ungroup() %>%
    arrange(.data$.orig_ind) %>%
    pull(.data$position)
}
