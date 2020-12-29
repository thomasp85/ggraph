#' Place nodes based on a multidimensional scaling of a set of pivot nodes
#'
#' This layout is similar to the 'mds' layout but uses only a subset of pivot
#' nodes for the mds calculation, making it considerably faster and thus suited
#' for large graphs
#'
#' @param graph A tbl_graph object
#' @param pivots The number of pivot nodes
#' @param weights An expression evaluated on the edge data to provide edge
#' weights for the layout. Currently ignored for the sparse version
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @references
#' Brandes, U. and Pich, C. (2006). *Eigensolver Methods for Progressive
#' Multidimensional Scaling of Large Data.* In International Symposium on Graph
#' Drawing (pp. 42-53). Springer
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_with_pmds
layout_tbl_graph_pmds <- function(graph, pivots, weights = NULL, circular = FALSE) {
  weights <- eval_tidy(enquo(weights), .E())
  if (is.null(weights)) weights <- NA
  xy <- layout_with_pmds(graph, pivots = pivots, weights = weights)
  nodes <- new_data_frame(list(x = xy[,1], y = xy[,2]))
  nodes$circular <- FALSE
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes
}
