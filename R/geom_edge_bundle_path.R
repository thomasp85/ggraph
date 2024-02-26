#' Bundle edges using edge path bundling
#'
#' This geom performs edge bundling using the edge path algorithm. This approach
#' uses the underlying graph structure to find shortest paths for each edge in
#' a graph the is gradually removed of it's edges. Since it is based on the
#' topology of the graph it should lead to less spurious bundling of unrelated
#' edges compared to [geom_edge_bundle_force()] and also has a simpler parameter
#' space.
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_force_path` and `geom_edge_force_path0` understand the following
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
#' `geom_edge_force_path2` understand the following aesthetics. Bold aesthetics are
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
#' `geom_edge_force_path` and `geom_edge_force_path2` furthermore takes the following
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
#' @param directed Logical. Should the shortest paths be calculated using
#' direction information of the graph. Setting this to `TRUE` can help split up
#' bundles that flows in opposite directions. Ignored for undirected graphs
#' @param max_distortion A multiplication factor to determine the maximum
#' allowed distortion of the path during bundling. If the new edge is longer
#' than `max_distortion` times the old length it is rejected.
#' @param weight_fac The exponent used to assign weights to the graph when
#' calculating the shortest path. The final weights are given as
#' `edge_length ^ weight_fac` meaning that sorter edges are prioritised when
#' calculating the weights.
#' @param tension A loosening factor when calculating the b-spline of the edge
#' based on the shortest path. Will move control points closer and closer to
#' the direct line as it approaches 0
#'
#' @author Thomas Lin Pedersen and David Schoch
#'
#' @family geom_edge_*
#'
#' @references
#' Wallinger, M., Archambault, D., Auber, D., Nöllenburg, M., and Peltonen, J.
#' (2022). *Edge-Path Bundling: A Less Ambiguous Edge Bundling Approach.* IEEE
#' Transactions on Visualization and Computer Graphics 28(1) 313-323.
#' https://doi.org/10.1109/TVCG.2021.3114795
#'
#' @rdname geom_edge_bundle_path
#' @name geom_edge_bundle_path
#'
#' @examples
#' ggraph(highschool) +
#'   geom_edge_bundle_path()
#'
#' # Use tension to lessen the effect
#' ggraph(highschool) +
#'   geom_edge_bundle_path(tension = 0.8)
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatEdgeBundlePath <- ggproto("StatEdgeBundlePath", Stat,
  setup_data = function(data, params) {
    StatEdgeBundlePath0$setup_data(data, params)
  },
  compute_panel = function(data, scales, n = 100, directed = NULL, max_distortion = 2,
                           weight_fac = 2, tension = 1) {
    edges <- StatEdgeBundlePath0$compute_panel(
      data, scales, directed = directed, max_distortion = max_distortion,
      weight_fac = weight_fac, tension = tension
    )
    StatBspline$compute_layer(edges, list(n = n), NULL)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'edge_id'),
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm")
)

#' @rdname geom_edge_bundle_path
#'
#' @export
geom_edge_bundle_path <- function(mapping = NULL, data = get_edges(),
                                  position = "identity", arrow = NULL,
                                  n = 100, directed = NULL, max_distortion = 2,
                                  weight_fac = 2, tension = 1,
                                  lineend = 'butt', linejoin = 'round', linemitre = 1,
                                  label_colour = 'black', label_alpha = 1,
                                  label_parse = FALSE, check_overlap = FALSE,
                                  angle_calc = 'rot', force_flip = TRUE,
                                  label_dodge = NULL, label_push = NULL,
                                  show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend, group = edge.id, edge_id = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBundlePath,
    geom = GeomEdgePath, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, n = n, interpolate = FALSE, directed = directed,
        max_distortion = max_distortion, weight_fac = weight_fac,
        tension = tension, label_colour = label_colour,
        label_alpha = label_alpha, label_parse = label_parse,
        check_overlap = check_overlap, angle_calc = angle_calc,
        force_flip = force_flip, label_dodge = label_dodge,
        label_push = label_push, ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatEdgeBundlePath2 <- ggproto("StatEdgeBundlePath2", Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop2(data)
  },
  compute_panel = function(data, scales, n = 100, directed = NULL, max_distortion = 2,
                           weight_fac = 2, tension = 1) {
    graph <- .G()
    nodes <- data_frame0(x = .N()$.ggraph_layout_x, y = .N()$.ggraph_layout_y)
    data <- data[order(data$group), ]
    edge_id <- data$edge_id[c(TRUE, FALSE)]
    edges <- path_bundle_mem(graph, nodes, .E()$from[edge_id], .E()$to[edge_id],
                             directed = directed, max_distortion = max_distortion,
                             weight_fac = weight_fac)
    if (tension < 1) edges <- relax(edges, tension)
    edges$PANEL <- data$PANEL[1]
    edges$group <- data$group[edges$group * 2]
    edges <- StatBspline$compute_layer(edges, list(n = n), NULL)
    extra_data <- data[1, !names(data) %in% c("x", "y", "group", "PANEL")][rep(NA, nrow(edges)), ]
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

#' @rdname geom_edge_bundle_path
#'
#' @export
geom_edge_bundle_path2 <- function(mapping = NULL, data = get_edges("long"),
                                    position = "identity", arrow = NULL,
                                    n = 100, directed = NULL, max_distortion = 2,
                                    weight_fac = 2, tension = 1,
                                    lineend = 'butt', linejoin = 'round', linemitre = 1,
                                    label_colour = 'black', label_alpha = 1,
                                    label_parse = FALSE, check_overlap = FALSE,
                                    angle_calc = 'rot', force_flip = TRUE,
                                    label_dodge = NULL, label_push = NULL,
                                    show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id, edge_id = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBundlePath2,
    geom = GeomEdgePath, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, n = n, interpolate = TRUE, directed = directed,
        max_distortion = max_distortion, weight_fac = weight_fac,
        tension = tension, label_colour = label_colour,
        label_alpha = label_alpha, label_parse = label_parse,
        check_overlap = check_overlap, angle_calc = angle_calc,
        force_flip = force_flip, label_dodge = label_dodge,
        label_push = label_push, ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeBundlePath0 <- ggproto('StatEdgeBundlePath0', Stat,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    remove_loop(data)
  },
  compute_panel = function(data, scales, directed = NULL, max_distortion = 2,
                           weight_fac = 2, tension = 1) {
    graph <- .G()
    nodes <- data_frame0(x = .N()$.ggraph_layout_x, y = .N()$.ggraph_layout_y)
    from <- .E()$from[data$edge_id]
    to <- .E()$to[data$edge_id]
    edges <- path_bundle_mem(graph, nodes, from, to, directed = directed,
                             max_distortion = max_distortion, weight_fac = weight_fac)
    if (tension < 1) edges <- relax(edges, tension)
    edges$PANEL <- data$PANEL[1]
    edges$group <- data$group[edges$group]
    cbind(edges, data[edges$group, !names(data) %in% c("x", "y", "xend", "yend", "PANEL", "group")])
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'edge_id'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm')
)
#' @rdname geom_edge_bundle_path
#'
#' @export
geom_edge_bundle_path0 <- function(mapping = NULL, data = get_edges(),
                                    position = "identity", arrow = NULL,
                                    directed = NULL, max_distortion = 2,
                                    weight_fac = 2, tension = 1,
                                    lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend, group = edge.id, edge_id = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBundlePath0,
    geom = GeomEdgeBspline, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow, lineend = lineend, directed = directed,
        max_distortion = max_distortion, weight_fac = weight_fac,
        tension = tension, ...
      )
    )
  )
}
#' @importFrom igraph gsize delete_edges shortest_paths is_directed as_edgelist
path_bundle <- function(graph, nodes, from, to, directed = directed, max_distortion = 2, weight_fac = 2) {
  m <- gsize(graph)
  lock <- rep(FALSE, m)
  skip <- rep(FALSE, m)

  edge_length <- sqrt((nodes$x[from] - nodes$x[to])^2 + (nodes$y[from] - nodes$y[to])^2)
  weights <- edge_length^weight_fac
  edges_order <- order(weights, decreasing = TRUE)
  paths <- vector("list", m)
  if (is_directed(graph)) {
    directed <- directed %||% TRUE
  } else if (!is.null(directed)) {
    cli::cli_warn("Ignoring {.arg directed} for undirected graphs")
  } else {
    directed <- FALSE
  }
  mode <- if (is.null(directed) || !directed) "all" else "out"

  all_edges <- as_edgelist(graph)
  if (directed) {
    all_edges <- paste(all_edges[,1], "-", all_edges[,2])
  } else {
    all_edges <- paste(pmin(all_edges[,1], all_edges[,2]), "-", pmax(all_edges[,1], all_edges[,2]))
  }
  # iterate
  for (e in edges_order) {
    s <- from[e]
    t <- to[e]
    paths[[e]] <- c(s, t)
    if (lock[e]) {
      next()
    }
    skip[e] <- TRUE
    g_temp <- delete_edges(graph, which(skip))
    path <- suppressWarnings(shortest_paths(g_temp, s, t, weights = weights[!skip], mode = mode, output = "vpath")$vpath[[1]])
    if (length(path) < 2) {
      skip[e] <- FALSE
      next()
    }
    path_length <- sum(sqrt((nodes$x[path[-length(path)]] - nodes$x[path[-1]])^2 + (nodes$y[path[-length(path)]] - nodes$y[path[-1]])^2))
    if (path_length >= max_distortion * edge_length[e]) {
      skip[e] <- FALSE
      next()
    }
    all_edges_on_path <- rep(as.integer(path), each = 2)
    all_edges_on_path <- matrix(all_edges_on_path[-c(1, length(all_edges_on_path))], ncol = 2, byrow = TRUE)
    if (directed) {
      all_edges_on_path <- paste(all_edges_on_path[,1], "-", all_edges_on_path[,2])
    } else {
      all_edges_on_path <- paste(pmin(all_edges_on_path[,1], all_edges_on_path[,2]),
                                 "-",
                                 pmax(all_edges_on_path[,1], all_edges_on_path[,2]))
    }
    all_edges_on_path <- all_edges %in% all_edges_on_path
    lock[all_edges_on_path] <- TRUE
    paths[[e]] <- path
  }
  ids <- rep(seq_along(from), lengths(paths))
  paths <- unlist(paths)
  data_frame0(x = nodes$x[paths], y = nodes$y[paths], group = ids)
}

path_bundle_mem <- memoise(path_bundle)
