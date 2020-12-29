#' Place nodes on their geographical space
#'
#' This layout is built for `sfnetwork` objects and is meant to
#' plot a graph on its geographical space, by extracting its X and Y coordinates
#'
#' @param graph A sfnetwork object
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular`.
#'
#' @family layout_tbl_graph_*
layout_tbl_graph_sf <- function(graph, circular = FALSE) {
  # Check the presence of sf.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package sf required, please install it first.", call. = FALSE)
  }
  graph <- activate(graph, "nodes")
  x <- sf::st_coordinates(graph)[,"X"]
  y <- sf::st_coordinates(graph)[,"Y"]
  nodes <- new_data_frame(list(x = x, y = y))
  # extra_data <- sf::st_drop_geometry(as_tibble(graph, active = "nodes"))
  # warn_dropped_vars(nodes, extra_data)
  # nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes$circular <- FALSE
  attr(nodes, 'graph') <- graph
  nodes
}
