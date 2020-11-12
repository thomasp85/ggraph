#' Place node to emphasize group structure
#'
#' This layout is optimised for drawing small-world types of graphs often found
#' in social networks, where distinct groups are still highly connected to the
#' remaining graph. Typical layouts struggle with this as they attempt to
#' minimise the edge length of all edges equally. The backbone layout is based
#' on weighing edges based on how well they hold together communities. The end
#' result is that communities tend to stick together despite high
#' interconnectivity.
#'
#' @param graph A tbl_graph object
#' @param keep The fraction of edges to use for creating the backbone
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#' Further an edge attribute called `backbone` is added giving whether the edge
#' was selected as backbone.
#'
#' @references
#' Nocaj, A., Ortmann, M., & Brandes, U. (2015). *Untangling the hairballs of
#' multi-centered, small-world online social media networks.* Journal of Graph
#' Algorithms and Applications: JGAA, 19(2), 595-618.
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_as_backbone
#'
layout_tbl_graph_backbone <- function(graph, keep = 0.2, circular = FALSE) {
  layout <- layout_as_backbone(graph, keep = keep, backbone = TRUE)
  xy <- layout$xy
  nodes <- new_data_frame(list(x = xy[,1], y = xy[,2]))
  nodes$circular <- FALSE
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  graph <- activate(graph, 'edges')
  graph <- mutate(graph, backbone = seq_len(graph_size()) %in% layout$backbone)
  attr(nodes, 'graph') <- activate(graph, 'nodes')
  nodes
}
