#' Create an unrooted layout using equal-angle or equal-daylight
#'
#' When drawing unrooted trees the standard dendrogram layout is a bad fit as it
#' implicitly creates a visual root node. Instead it is possible to spread the
#' leafs out on the plane without putting any special emphasis on a particular
#' node using an unrooted layout. The standard algorithm is the equal angle
#' algorithm, but it can struggle with optimising the leaf distribution for
#' large trees trees with very uneven branch length. The equal daylight
#' algorithm modifies the output of the equal angle algorithm to better disperse
#' the leaves, at the cost of higher computational cost and the possibility of
#' edge crossings for very large unbalanced trees. For standard sized trees the
#' daylight algorithm is far superior and not too heavy so it is the default.
#'
#' @note
#' Unrooted is a layout intended for undirected trees, that is, graphs with no
#' cycles. If the provided graph does not fit this format an attempt to convert
#' it to such a format will be made.
#'
#' @param graph A tbl_graph object
#' @param daylight Should equal-daylight adjustments be made
#' @param length An expression evaluating to the branch length of each edge
#' @param tolerance The threshold for mean angular adjustment before terminating
#' the daylight adjustment
#' @param rotation_mod A modifier for the angular adjustment of each branch. Set
#' it below 1 to let the daylight adjustment progress more slowly
#' @param maxiter The maximum number of iterations in the the daylight
#' adjustment
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular`, `leaf` as well as
#' any information stored as node variables in the tbl_graph object.
#'
#' @references
#' Felsenstein, J. (2004) *Drawing Trees*, in Inferring Phylogenies. Sinauer
#' Assoc., pp 573-584
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph as.undirected gorder
layout_tbl_graph_unrooted <- function(graph, daylight = TRUE, length = NULL, tolerance = 0.05, rotation_mod = 1, maxiter = 100, circular = FALSE) {
  extra_data <- as_tibble(graph, active = 'nodes')
  graph <- as.undirected(graph)
  length <- enquo(length)
  length <- eval_tidy(length, .E())
  hierarchy <- tree_to_hierarchy(graph, 'out', seq_len(gorder(graph)), weight = NULL, length)
  layout <- unrooted(hierarchy$parent, hierarchy$order, hierarchy$height, daylight, tolerance, rotation_mod, maxiter)[-1, ]
  layout <- new_data_frame(list(
    x = layout[, 1],
    y = layout[, 2],
    circular = FALSE,
    leaf = degree(graph) == 1
  ))
  warn_dropped_vars(layout, extra_data)
  layout <- cbind(layout, extra_data[, !names(extra_data) %in% names(layout), drop = FALSE])
  layout
}
