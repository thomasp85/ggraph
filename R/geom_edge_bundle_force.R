#' Bundle edges using force directed edge bundling
#'
#' This geom performs force directed edge bundling to reduce visual clutter.
#' It uses a self-organizing approach to bundling in which edges are modeled as
#' flexible springs that can attract each other without the need of a hierarchy.
#' Be aware that this bundling technique works globally and thus may bundle
#' edges that is otherwise unrelated together. Care should be taken when
#' interpreting the resulting visual. An alternative approach to edge bundling
#' that uses the graph topology is provided by [geom_edge_bundle_path()].
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_bundle_force` and `geom_edge_bundle_force0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overwritten.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_bundle_force2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overwritten.
#'
#' - **x**
#' - **y**
#' - **group**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_bundle_force` and `geom_edge_bundle_force2` furthermore takes the following
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
#' @param force The spring force during bundling
#' @param n_cycle number of iteration cycles
#' @param cuts_start initial number of edge divisions
#' @param step initial step size
#' @param cuts_new factor for how many new division points to add after a cycle
#' @param n_iter number of iteration steps per cycle
#' @param iter_new factor of how to decrease the number of iterations per cycle
#' @param threshold threshold for considering two edges to be interacting
#' @param eps tolerance
#'
#' @author David Schoch
#'
#' @family geom_edge_*
#'
#' @references
#' Holten, D. and Wijk, J.J.V. (2009). *Force‐Directed Edge Bundling for Graph
#' Visualization.* Computer Graphics Forum (Blackwell Publishing Ltd) 28, no. 3:
#' 983-990. https://doi.org/10.1111/j.1467-8659.2009.01450.x
#'
#' @rdname geom_edge_bundle_force
#' @name geom_edge_bundle_force
#'
#' @examples
#' # (not necessarily an insightful use)
#' ggraph(highschool) +
#'   geom_edge_bundle_force(n_cycle = 2, threshold = 0.4)
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatEdgeBundleForce <- ggproto(
  "StatEdgeBundleForce",
  Stat,
  setup_data = function(data, params) {
    StatEdgeBundleForce0$setup_data(data, params)
  },
  compute_panel = function(
    data,
    scales,
    n = 100,
    force = 1,
    n_cycle = 6,
    cuts_start = 1,
    step = 0.04,
    cuts_new = 2,
    n_iter = 50,
    iter_new = 2 / 3,
    threshold = 0.6,
    eps = 1e-8
  ) {
    edges <- StatEdgeBundleForce0$compute_panel(
      data,
      scales,
      force = force,
      n_cycle = n_cycle,
      cuts_start = cuts_start,
      step = step,
      cuts_new = cuts_new,
      n_iter = n_iter,
      iter_new = iter_new,
      threshold = threshold,
      eps = eps
    )
    StatBspline$compute_layer(edges, list(n = n), NULL)
  },
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm")
)

#' @rdname geom_edge_bundle_force
#'
#' @export
geom_edge_bundle_force <- function(
  mapping = NULL,
  data = get_edges(),
  position = "identity",
  arrow = NULL,
  n = 100,
  force = 1,
  n_cycle = 6,
  cuts_start = 1,
  step = 0.04,
  cuts_new = 2,
  n_iter = 50,
  iter_new = 2 / 3,
  threshold = 0.6,
  eps = 1e-8,
  lineend = 'butt',
  linejoin = 'round',
  linemitre = 1,
  label_colour = 'black',
  label_alpha = 1,
  label_parse = FALSE,
  check_overlap = FALSE,
  angle_calc = 'rot',
  force_flip = TRUE,
  label_dodge = NULL,
  label_push = NULL,
  show.legend = NA,
  ...
) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(
    mapping,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleForce,
    geom = GeomEdgePath,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        n = n,
        interpolate = FALSE,
        force = force,
        n_cycle = n_cycle,
        cuts_start = cuts_start,
        step = step,
        cuts_new = cuts_new,
        n_iter = n_iter,
        iter_new = iter_new,
        threshold = threshold,
        eps = eps,
        label_colour = label_colour,
        label_alpha = label_alpha,
        label_parse = label_parse,
        check_overlap = check_overlap,
        angle_calc = angle_calc,
        force_flip = force_flip,
        label_dodge = label_dodge,
        label_push = label_push,
        ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatEdgeBundleForce2 <- ggproto(
  "StatEdgeBundleForce2",
  Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop2(data)
  },
  compute_panel = function(
    data,
    scales,
    n = 100,
    force = 1,
    n_cycle = 6,
    cuts_start = 1,
    step = 0.04,
    cuts_new = 2,
    n_iter = 50,
    iter_new = 2 / 3,
    threshold = 0.6,
    eps = 1e-8
  ) {
    data <- data[order(data$group), ]
    edges <- cbind(
      data$x[c(TRUE, FALSE)],
      data$y[c(TRUE, FALSE)],
      data$x[c(FALSE, TRUE)],
      data$y[c(FALSE, TRUE)]
    )
    edges <- force_bundle_mem(
      edges,
      K = force,
      C = n_cycle,
      P = cuts_start,
      S = step,
      P_rate = cuts_new,
      I = n_iter,
      I_rate = iter_new,
      compatibility_threshold = threshold,
      eps = eps
    )
    edges$PANEL <- data$PANEL[1]
    edges$group <- data$group[edges$group * 2]
    edges <- StatBspline$compute_layer(edges, list(n = n), NULL)
    extra_data <- data[1, !names(data) %in% c("x", "y", "group", "PANEL")][
      rep(NA, nrow(edges)),
    ]
    edges$.interp <- TRUE
    ends <- !duplicated(edges$group) | !duplicated(edges$group, fromLast = TRUE)
    edges$.interp[ends] <- FALSE
    extra_data[ends, ] <- data[, names(extra_data)]
    cbind(edges, extra_data)
  },
  required_aes = c("x", "y"),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm")
)

#' @rdname geom_edge_bundle_force
#'
#' @export
geom_edge_bundle_force2 <- function(
  mapping = NULL,
  data = get_edges("long"),
  position = "identity",
  arrow = NULL,
  n = 100,
  force = 1,
  n_cycle = 6,
  cuts_start = 1,
  step = 0.04,
  cuts_new = 2,
  n_iter = 50,
  iter_new = 2 / 3,
  threshold = 0.6,
  eps = 1e-8,
  lineend = 'butt',
  linejoin = 'round',
  linemitre = 1,
  label_colour = 'black',
  label_alpha = 1,
  label_parse = FALSE,
  check_overlap = FALSE,
  angle_calc = 'rot',
  force_flip = TRUE,
  label_dodge = NULL,
  label_push = NULL,
  show.legend = NA,
  ...
) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(
    mapping,
    aes(
      x = x,
      y = y,
      group = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleForce2,
    geom = GeomEdgePath,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        n = n,
        interpolate = TRUE,
        force = force,
        n_cycle = n_cycle,
        cuts_start = cuts_start,
        step = step,
        cuts_new = cuts_new,
        n_iter = n_iter,
        iter_new = iter_new,
        threshold = threshold,
        eps = eps,
        label_colour = label_colour,
        label_alpha = label_alpha,
        label_parse = label_parse,
        check_overlap = check_overlap,
        angle_calc = angle_calc,
        force_flip = force_flip,
        label_dodge = label_dodge,
        label_push = label_push,
        ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeBundleForce0 <- ggproto(
  'StatEdgeBundleForce0',
  Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop(data)
  },
  compute_panel = function(
    data,
    scales,
    force = 1,
    n_cycle = 6,
    cuts_start = 1,
    step = 0.04,
    cuts_new = 2,
    n_iter = 50,
    iter_new = 2 / 3,
    threshold = 0.6,
    eps = 1e-8
  ) {
    edges <- cbind(data$x, data$y, data$xend, data$yend)
    edges <- force_bundle_mem(
      edges,
      K = force,
      C = n_cycle,
      P = cuts_start,
      S = step,
      P_rate = cuts_new,
      I = n_iter,
      I_rate = iter_new,
      compatibility_threshold = threshold,
      eps = eps
    )
    edges$PANEL <- data$PANEL[1]
    edges$group <- data$group[edges$group]
    cbind(
      edges,
      data[
        edges$group,
        !names(data) %in% c("x", "y", "xend", "yend", "PANEL", "group")
      ]
    )
  },
  required_aes = c('x', 'y', 'xend', 'yend'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm')
)
#' @rdname geom_edge_bundle_force
#'
#' @export
geom_edge_bundle_force0 <- function(
  mapping = NULL,
  data = get_edges(),
  position = "identity",
  arrow = NULL,
  force = 1,
  n_cycle = 6,
  cuts_start = 1,
  step = 0.04,
  cuts_new = 2,
  n_iter = 50,
  iter_new = 2 / 3,
  threshold = 0.6,
  eps = 1e-8,
  lineend = 'butt',
  show.legend = NA,
  ...
) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(
    mapping,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleForce0,
    geom = GeomEdgeBspline,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow,
        lineend = lineend,
        force = force,
        n_cycle = n_cycle,
        cuts_start = cuts_start,
        step = step,
        cuts_new = cuts_new,
        n_iter = n_iter,
        iter_new = iter_new,
        threshold = threshold,
        eps = eps,
        ...
      )
    )
  )
}

force_bundle <- function(
  data,
  K,
  C,
  P,
  S,
  P_rate,
  I,
  I_rate,
  compatibility_threshold,
  eps
) {
  # initialize matrix with coordinates
  m <- nrow(data)

  mode(data) <- 'numeric'

  # main force bundling routine
  force_bundle_iter(
    data,
    as.numeric(K),
    as.integer(C),
    as.integer(P),
    as.integer(P_rate),
    as.numeric(S),
    as.integer(I),
    as.numeric(I_rate),
    as.numeric(compatibility_threshold),
    as.numeric(eps)
  )
}

force_bundle_mem <- memoise(force_bundle)
