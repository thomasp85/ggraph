#' Place nodes on a diagonal
#'
#' This layout puts all nodes on a diagonal, thus preparing the layout for use
#' with [geom_edge_point()] resulting in a matrix layout. While matrix
#' layouts excel in scalability, the interpretation of the visual is very
#' dependent on the sorting of the nodes. Different sorting algorithms have been
#' implemented in `tidygraph` and these can be used directly. Behrisch
#' *et al.* (2016) have provided a nice overview of some of the different
#' sorting algorithms and what insight they might bring, along with a rundown of
#' different patterns to look out for.
#'
#' @param graph An `tbl_graph` object
#'
#' @param circular Ignored
#'
#' @param sort.by An expression providing the sorting of the nodes. If `NULL`
#' the nodes will be ordered by their index in the graph.
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder
#' @importFrom rlang enquo eval_tidy
#'
#' @references
#' Behrisch, M., Bach, B., Riche, N. H., Schreck, T., Fekete, J.-D. (2016).
#' *Matrix Reordering Methods for Table and Network Visualization*.
#' Computer Graphics Forum, 35: 693–716. <https://doi.org/10.1111/cgf.12935>
#'
layout_tbl_graph_matrix <- function(graph, circular = FALSE, sort.by = NULL) {
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  if (!is.null(sort.by)) {
    pos <- order(order(sort.by))
  } else {
    pos <- seq_len(gorder(graph))
  }
  nodes <- new_data_frame(list(x = pos, y = pos))
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes$circular <- FALSE
  nodes
}
