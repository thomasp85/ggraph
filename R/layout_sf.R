#' Place nodes on their geographical space
#'
#' This layout is built for objects of class `sfnetwork` and is meant to
#' plot a graph on its geographical space, by extracting its X and Y coordinates
#'
#' @param graph An sfnetwork object
#' @param circular ignored
#'
#' @importFrom tidygraph as_tbl_graph
#'
#' @return A data.frame with the columns `x`, `y`, `circular`.
#'
#' @family layout_tbl_graph_*
layout_tbl_graph_sf <- function(graph, circular = FALSE) {
  # Check the presence of sf.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package sf required, please install it first.", call. = FALSE)
  }
  # Check if network is an sfnetwork
  if (!inherits(graph, "sfnetwork")) {
    stop("Layout 'sf' needs an 'sfnetwork' graph.", call. = FALSE)
  }
  # Extract X and Y coordinates from the nodes
  graph <- activate(graph, "nodes")
  x <- sf::st_coordinates(graph)[,"X"]
  y <- sf::st_coordinates(graph)[,"Y"]
  # Create layout data frame
  nodes <- new_data_frame(list(x = x, y = y))
  nodes$circular <- FALSE
  # Convert to tbl_graph to 'unstick' geometry column
  extra_data <- as_tibble(as_tbl_graph(graph), active = "nodes")
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  attr(nodes, 'graph') <- graph
  nodes
}
