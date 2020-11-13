#' Calculate nodes as rectangles subdividing that of their parent
#'
#' A treemap is a space filling hierarchical layout that maps nodes to
#' rectangles. The rectangles of the children of a node is packed into the
#' rectangle of the node so that the size of a rectangle is a function of the
#' size of the children. The size of the leaf nodes can be mapped arbitrarily
#' (defaults to 1). Many different algorithms exists for dividing a rectangle
#' into smaller bits, some optimizing the aspect ratio and some focusing on the
#' ordering of the rectangles. See details for more discussions on this. The
#' treemap layout was first developed by Ben Shneiderman for visualizing disk
#' usage in the early '90 and has seen many improvements since.
#'
#' @details
#' Different approaches to dividing the rectangles in a treemap exists; all with
#' their strengths and weaknesses. Currently only the split algorithm is
#' implemented which strikes a good balance between aspect ratio and order
#' preservation, but other, more well-known, algorithms such as squarify and
#' slice-and-dice will eventually be implemented.
#'
#' \strong{Algorithms}
#'
#' *Split* (default)
#'
#' The Split algorithm was developed by Bjorn Engdahl in order to address the
#' downsides of both the original slice-and-dice algorithm (poor aspect ratio)
#' and the popular squarify algorithm (no ordering of nodes). It works by
#' finding the best cut in the ordered list of children in terms of making sure
#' that the two rectangles associated with the split will have optimal aspect
#' ratio.
#'
#'
#' @note
#' Treemap is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph A `tbl_graph` object
#'
#' @param algorithm The name of the tiling algorithm to use. Defaults to 'split'
#'
#' @param weight An optional node variable to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored.
#'
#' @param sort.by The name of a node variables to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. `'out'` (default)
#' means that parents point towards their children, while `'in'` means that
#' children point towards their parent.
#'
#' @param height The height of the bounding rectangle
#'
#' @param width The width of the bounding rectangle
#'
#' @return A data.frame with the columns `x`, `y`, `width`,
#' `height`, `leaf`, `depth`, `circular` as well as any
#' information stored as node variables in the tbl_graph object.
#'
#' @references
#' Engdahl, B. (2005). *Ordered and unordered treemap algorithms and their
#' applications on handheld devices*. Master's Degree Project.
#'
#' Johnson, B., & Ben Shneiderman. (1991). *Tree maps: A Space-Filling
#' Approach to the Visualization of Hierarchical Information Structures*. IEEE
#' Visualization, 284-291. <https://doi.org/10.1109/VISUAL.1991.175815>
#'
#' @family layout_tbl_graph_*
#'
layout_tbl_graph_treemap <- function(graph, algorithm = 'split', weight = NULL, circular = FALSE, sort.by = NULL, direction = 'out', height = 1, width = 1) {
  weight <- enquo(weight)
  weight <- eval_tidy(weight, .N())
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by)
  hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight)
  layout <- switch(
    algorithm,
    split = splitTreemap(hierarchy$parent, hierarchy$order, hierarchy$weight, width, height),
    stop('Unknown algorithm')
  )[-1, ]
  layout <- new_data_frame(list(
    x = layout[, 1] + layout[, 3] / 2,
    y = layout[, 2] + layout[, 4] / 2,
    width = layout[, 3],
    height = layout[, 4],
    circular = FALSE,
    leaf = degree(graph, mode = direction) == 0,
    depth = node_depth(graph, mode = direction)
  ))
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(layout, extra_data)
  layout <- cbind(layout, extra_data[, !names(extra_data) %in% names(layout), drop = FALSE])
  layout
}
