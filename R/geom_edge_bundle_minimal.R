#' Bundle edges along the minimal spanning tree
#'
#' This geom performs edge bundling by letting edges follow the shortest path
#' along the minimal spanning tree of the graph. Due to it's simplicity it is
#' very fast but does enforce a tree-like appearance to the bundling. Adjusting
#' the `max_distortion` and `tension` parameters may alleviate this to some
#' extend.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_force_minimal` and `geom_edge_force_minimal0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overwritten.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **edge_id** (should not be overwritten)
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_force_minimal2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overwritten.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **edge_id** (should not be overwritten)
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_force_minimal` and `geom_edge_force_minimal2` furthermore takes the following
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
#' @inheritParams geom_edge_bundle_path
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @rdname geom_edge_bundle_minimal
#' @name geom_edge_bundle_minimal
#'
#' @examples
#' ggraph(highschool) +
#'   geom_edge_bundle_minimal()
#'
#' # Allow more edges to bundle
#' ggraph(highschool) +
#'   geom_edge_bundle_minimal(max_distortion = 5, tension = 0.9)
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatEdgeBundleMinimal <- ggproto(
  "StatEdgeBundleMinimal",
  Stat,
  setup_data = function(data, params) {
    StatEdgeBundleMinimal0$setup_data(data, params)
  },
  compute_panel = function(
    data,
    scales,
    n = 100,
    max_distortion = 2,
    weight_fac = 2,
    tension = 1
  ) {
    edges <- StatEdgeBundleMinimal0$compute_panel(
      data,
      scales,
      max_distortion = max_distortion,
      weight_fac = weight_fac,
      tension = tension
    )
    StatBspline$compute_layer(edges, list(n = n), NULL)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'edge_id'),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm")
)

#' @rdname geom_edge_bundle_minimal
#'
#' @export
geom_edge_bundle_minimal <- function(
  mapping = NULL,
  data = get_edges(),
  position = "identity",
  arrow = NULL,
  n = 100,
  max_distortion = 2,
  weight_fac = 2,
  tension = 1,
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
      group = edge.id,
      edge_id = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleMinimal,
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
        max_distortion = max_distortion,
        weight_fac = weight_fac,
        tension = tension,
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
StatEdgeBundleMinimal2 <- ggproto(
  "StatEdgeBundleMinimal2",
  Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop2(data)
  },
  compute_panel = function(
    data,
    scales,
    n = 100,
    max_distortion = 2,
    weight_fac = 2,
    tension = 1
  ) {
    graph <- .G()
    nodes <- data_frame0(x = .N()$.ggraph_layout_x, y = .N()$.ggraph_layout_y)
    data <- data[order(data$group), ]
    edge_id <- data$edge_id[c(TRUE, FALSE)]
    edges <- minimal_bundle_mem(
      graph,
      nodes,
      .E()$from[edge_id],
      .E()$to[edge_id],
      max_distortion = max_distortion,
      weight_fac = weight_fac
    )
    if (tension < 1) {
      edges <- relax(edges, tension)
    }
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
  required_aes = c("x", "y", "edge_id"),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm")
)

#' @rdname geom_edge_bundle_minimal
#'
#' @export
geom_edge_bundle_minimal2 <- function(
  mapping = NULL,
  data = get_edges("long"),
  position = "identity",
  arrow = NULL,
  n = 100,
  max_distortion = 2,
  weight_fac = 2,
  tension = 1,
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
      group = edge.id,
      edge_id = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleMinimal2,
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
        max_distortion = max_distortion,
        weight_fac = weight_fac,
        tension = tension,
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
StatEdgeBundleMinimal0 <- ggproto(
  'StatEdgeBundleMinimal0',
  Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop(data)
  },
  compute_panel = function(
    data,
    scales,
    max_distortion = 2,
    weight_fac = 2,
    tension = 1
  ) {
    graph <- .G()
    nodes <- data_frame0(x = .N()$.ggraph_layout_x, y = .N()$.ggraph_layout_y)
    edges <- minimal_bundle_mem(
      graph,
      nodes,
      .E()$from[data$edge_id],
      .E()$to[data$edge_id],
      max_distortion = max_distortion,
      weight_fac = weight_fac
    )
    if (tension < 1) {
      edges <- relax(edges, tension)
    }
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
  required_aes = c('x', 'y', 'xend', 'yend', 'edge_id'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm')
)
#' @rdname geom_edge_bundle_minimal
#'
#' @export
geom_edge_bundle_minimal0 <- function(
  mapping = NULL,
  data = get_edges(),
  position = "identity",
  arrow = NULL,
  max_distortion = 2,
  weight_fac = 2,
  tension = 1,
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
      group = edge.id,
      edge_id = edge.id
    )
  )
  layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeBundleMinimal0,
    geom = GeomEdgeBspline,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow,
        lineend = lineend,
        max_distortion = max_distortion,
        weight_fac = weight_fac,
        tension = tension,
        ...
      )
    )
  )
}
#' @importFrom igraph mst shortest_paths
minimal_bundle <- function(
  graph,
  nodes,
  from,
  to,
  max_distortion = 2,
  weight_fac = 2
) {
  edge_length <- sqrt(
    (nodes$x[from] - nodes$x[to])^2 + (nodes$y[from] - nodes$y[to])^2
  )
  weights <- edge_length^weight_fac
  g_temp <- mst(graph, weights)
  paths <- lapply(seq_along(from), function(f) {
    s <- from[f]
    t <- to[f]
    path <- suppressWarnings(shortest_paths(
      g_temp,
      s,
      t,
      mode = "all",
      output = "vpath"
    )$vpath[[1]])
    path_length <- sum(sqrt(
      (nodes$x[path[-length(path)]] - nodes$x[path[-1]])^2 +
        (nodes$y[path[-length(path)]] - nodes$y[path[-1]])^2
    ))
    if (path_length >= max_distortion * edge_length[f]) {
      c(s, t)
    } else {
      path
    }
  })
  ids <- rep(seq_along(from), lengths(paths))
  paths <- unlist(paths)
  data_frame0(x = nodes$x[paths], y = nodes$y[paths], group = ids)
}

minimal_bundle_mem <- memoise(minimal_bundle)
