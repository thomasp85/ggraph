#' Calculate nodes as fractal circle buds
#'
#' The cactustree layout is a hierarchical layout optimised for use with
#' hierarchical edge bundling ([geom_conn_bundle()]). It is a fractal layout
#' that places node children as circles on the periphery of their parent circle,
#' each circle scaled by the total weight of their children.
#'
#' @note
#' cactustree is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An `tbl_graph` object
#'
#' @param direction The direction of the tree in the graph. `'out'` (default)
#' means that parents point towards their children, while `'in'` means that
#' children point towards their parent.
#'
#' @param weight An optional node variable to use as weight. If `NULL` all leaf
#' nodes will be assigned a weight of `1`.
#'
#' @param scale_factor A scaling factor for the circles in the layout. The
#' radius will be calculated as `total_weight ^ scale_factor` with `total_weight`
#' being the weight of the node and all it's children.
#'
#' @param overlap How much is the center of child nodes offset from the
#' periphery of their parent as a proportion of their own radius.
#'
#' @param upright Logical. Should the children of the root only be distributed
#' around the top half of the root or all the way around.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored.
#'
#' @return A data.frame with the columns `x`, `y`, `r`, `leaf`, `depth`,
#' `circular` as well as any information stored as node variables in the
#' tbl_graph object.
#'
#' @references
#' Dang, T., Forbes, A. (2017). *CactusTree: A Tree Drawing Approach
#' for Hierarchical Edge Bundling*. 2017 IEEE Pacific Visualization Symposium,
#' 210-214. https://doi.org/10.1109/PACIFICVIS.2017.8031596
#'
#' @family layout_tbl_graph_*
#'
layout_tbl_graph_cactustree <- function(graph, direction = "out", weight = NULL, scale_factor = 0.75, overlap = 0.5, upright = FALSE, circular = FALSE) {
  weight <- enquo(weight)
  weight <- eval_tidy(weight, .N())
  if (is.null(weight)) {
    weight <- as.numeric(degree(graph, mode = direction) == 0)
  }
  hierarchy <- tree_to_hierarchy(graph, direction, NULL, weight, NULL)
  layout <- cactusTree(
    as.integer(hierarchy$parent),
    as.integer(hierarchy$order),
    as.numeric(hierarchy$weight),
    as.numeric(scale_factor),
    as.numeric(overlap),
    as.logical(upright)
  )[-1, ]
  nodes <- data_frame0(
    x = layout[, 1],
    y = layout[, 2],
    r = layout[, 3],
    circular = FALSE,
    leaf = degree(graph, mode = direction) == 0,
    depth = node_depth(graph, mode = direction)
  )
  nodes <- combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
  attr(nodes, 'graph') <- add_direction(graph, nodes)
  nodes
}
