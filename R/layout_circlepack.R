#' Calculate nodes as circles packed within their parent circle
#'
#' The circle packing algorithm is basically a treemap using circles instead of
#' rectangles. Due to the nature of circles they cannot be packed as efficiently
#' leading to increased amount of "empty space" as compared to a treemap. This
#' can be beneficial though, as the added empty space can aid in visually
#' showing the hierarchy.
#'
#' @details
#' The circle packing is based on the algorithm developed by Weixin Wang and
#' collaborators which tries to find the most dense packing of circles as they
#' are added, one by one. This makes the algorithm very dependent on the order
#' in which circles are added and it is possible that layouts could sometimes
#' be optimized by choosing a different ordering. The algorithm for finding the
#' enclosing circle is the randomized incremental algorithm proposed by Emo
#' Welzl. Both of the above algorithms are the same as used in the D3.js
#' implementation of circle packing and their C++ implementation in ggraph is
#' inspired by Mike Bostocks JavaScript implementation.
#'
#' @note
#' Circle packing is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An `tbl_graph` object
#'
#' @param weight An optional node variable to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored.
#'
#' @param sort.by The name of a node variable to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. `'out'` (default)
#' means that parents point towards their children, while `'in'` means that
#' children point towards their parent.
#'
#' @return A data.frame with the columns `x`, `y`, `r`, `leaf`,
#' `depth`, `circular` as well as any information stored as node
#' variables in the tbl_graph object.
#'
#' @references
#' Wang, W., Wang, H. H., Dai, G., & Wang, H. (2006). *Visualization of
#' large hierarchical data by circle packing*. Chi, 517-520.
#'
#' Welzl, E. (1991). *Smallest enclosing disks (balls and ellipsoids)*. New
#' Results and New Trends in Computer Science, 359-370.
#'
#' @family layout_tbl_graph_*
#'
layout_tbl_graph_circlepack <- function(graph, weight = NULL, circular = FALSE, sort.by = NULL, direction = 'out') {
  weight <- enquo(weight)
  weight <- eval_tidy(weight, .N())
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight)
  layout <- circlePackLayout(hierarchy$parent, hierarchy$weight)[-1, ]
  layout <- new_data_frame(list(
    x = layout[, 1],
    y = layout[, 2],
    r = layout[, 3],
    circular = FALSE,
    leaf = degree(graph, mode = direction) == 0,
    depth = node_depth(graph, mode = direction)
  ))
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(layout, extra_data)
  layout <- cbind(layout, extra_data[, !names(extra_data) %in% names(layout), drop = FALSE])
  layout
}
