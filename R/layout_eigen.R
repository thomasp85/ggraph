#' Place nodes according to their eigenvalues
#'
#' This layout is based on the idea of spectral layouts where node coordinates
#' are calculated directly by decomposing a matrix representation of the graph
#' and extracting the eigenvectors.
#'
#' @param graph A tbl_graph object
#' @param type The type of matrix to extract the eigenvectors from. Either
#' `'laplacian'` or `'adjacency'`
#' @param eigenvector The eigenvector to use for coordinates. Either `'smallest'`
#' or `'largest'`
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_with_eigen
layout_tbl_graph_eigen <- function(graph, type = 'laplacian', eigenvector = 'smallest', circular = FALSE) {
  type <- match.arg(type, c('laplacian', 'adjacency'))
  eigenvector <- match.arg(eigenvector, c('smallest', 'largest'))
  xy <- layout_with_eigen(graph, type = type, ev = eigenvector)
  nodes <- new_data_frame(list(x = xy[,1],y = xy[,2]))
  nodes$circular <- FALSE
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes
}
