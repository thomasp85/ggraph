#' Apply a dendrogram layout to layout_tbl_graph
#'
#' This layout mimics the [igraph::layout_as_tree()] algorithm
#' supplied by igraph, but puts all leaves at 0 and builds it up from there,
#' instead of starting from the root and building it from there. The height of
#' branch points are related to the maximum distance to an edge from the branch
#' node, or read from a node variable.
#'
#' @note This function is not intended to be used directly but by setting
#' `layout = 'dendrogram'` in [create_layout()]
#'
#' @param graph A `tbl_graph` object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to `FALSE`.
#'
#' @param offset If `circular = TRUE`, where should it begin. Defaults to
#' `pi/2` which is equivalent to 12 o'clock.
#'
#' @param height The node variable holding the height of each node in the
#' dendrogram. If `NULL` it will be calculated as the maximal distance to a
#' leaf.
#'
#' @param length An edge parameter giving the length of each edge. The node
#' height will be calculated from the maximal length to the root node (ignored
#' if `height` does not evaluate to `NULL`)
#'
#' @param repel Should leafs repel each other relative to the height of their
#' common ancestor. Will emphasize clusters
#'
#' @param ratio The strength of repulsion if `repel = TRUE`. Higher values will
#' give more defined clusters
#'
#' @param direction The direction to the leaves. Defaults to 'out'
#'
#' @return A data.frame with the columns `x`, `y`, `circular`, `depth` and
#' `leaf` as well as any information stored as node variables on the
#' tbl_graph
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder degree adjacent_vertices distances neighbors
#' @importFrom tidygraph node_is_root
#' @importFrom rlang enquo eval_tidy
#'
layout_tbl_graph_dendrogram <- function(graph, circular = FALSE, offset = pi / 2, height = NULL, length = NULL, repel = FALSE, ratio = 1, direction = 'out') {
  height <- enquo(height)
  length <- enquo(length)
  reverse_dir <- if (direction == 'out') 'in' else 'out'
  if (quo_is_null(height)) {
    if (quo_is_null(length)) {
      height <- NA
    } else {
      length <- eval_tidy(length, .E())
      full_lengths <- distances(graph, to = node_is_root(), weights = length, mode = reverse_dir)
      full_lengths[is.infinite(full_lengths)] <- 0
      height <- unname(apply(full_lengths, 1, max))
      height <- abs(height - max(height))
    }
  } else {
    height <- eval_tidy(height, .N())
  }
  nodes <- data_frame0(
    x = rep(NA_real_, gorder(graph)),
    y = height,
    leaf = degree(graph, mode = direction) == 0
  )
  if (all(is.na(nodes$y))) {
    nodes$y <- vapply(seq_len(gorder(graph)), function(i) {
      max(bfs(graph, i, direction, unreachable = FALSE, order = FALSE, dist = TRUE)$dist, na.rm = TRUE)
    }, numeric(1))
  }
  if (repel) {
    pad <- min(nodes$y[nodes$y != 0]) / 2
  } else {
    pad <- 0
  }
  startnode <- which(degree(graph, mode = reverse_dir) == 0)
  if (length(startnode) < 1) cli::cli_abort('The graph doesn\'t contain a root node')
  neighbors <- lapply(adjacent_vertices(graph, seq_len(gorder(graph)), direction), as.integer)
  nodes$x <- dendrogram_spread(neighbors, startnode, nodes$y, nodes$leaf, repel, pad, ratio)
  graph <- add_direction(graph, nodes)
  if (circular) {
    radial <- radial_trans(
      r.range = rev(range(nodes$y)),
      a.range = range(nodes$x),
      offset = offset
    )
    coords <- radial$transform(nodes$y, nodes$x)
    nodes$x <- coords$x
    nodes$y <- coords$y
  }
  nodes <- combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
  nodes$circular <- circular
  attr(nodes, 'graph') <- graph
  nodes
}
