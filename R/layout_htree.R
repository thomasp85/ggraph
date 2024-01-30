#' Layout binary trees in a fractal H formation
#'
#' This is a spac efficient layout only useful for binary trees. It is fractal
#' and works by offsetting child nodes from their parent either horizontally or
#' vertically depending on depth. The offset is decreased at each step by a
#' factor of the square root of 2.
#'
#' @note
#' H Tree is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An `tbl_graph` object
#'
#' @param sort.by The name of a node variable to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. `'out'` (default)
#' means that parents point towards their children, while `'in'` means that
#' children point towards their parent.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored
#'
#' @return A data.frame with the columns `x`, `y`, `leaf`, `depth`, `circular`
#' as well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph degree count_components
#'
layout_tbl_graph_htree <- function(graph, sort.by = NULL, direction = 'out', circular = FALSE) {
  if (any(degree(graph, mode = direction) > 2)) {
    cli::cli_abort("H-Tree layouts can only be used with binary trees")
  }
  if (count_components(graph, "weak") != 1) {
    cli::cli_abort("H-Tree layouts can only be used with fully connected graphs")
  }
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  hierarchy <- tree_to_hierarchy(graph, direction, sort.by, NULL)
  layout <- hTree(as.integer(hierarchy$parent), as.integer(hierarchy$order))[-1, ]
  nodes <- data_frame0(
    x = layout[, 1],
    y = layout[, 2],
    circular = FALSE
  )
  nodes$leaf <- degree(graph, mode = direction) == 0
  nodes$depth <- node_depth(graph, mode = direction)
  nodes <- combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
  attr(nodes, 'graph') <- add_direction(graph, nodes)
  nodes
}
