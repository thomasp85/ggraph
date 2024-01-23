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
  check_installed("sf")
  # Check if network is an sfnetwork
  if (!inherits(graph, "sfnetwork")) {
    cli::cli_abort("The 'sf' layout needs an {.cls sfnetwork} graph.")
  }
  # Extract X and Y coordinates from the nodes
  graph <- activate(graph, "nodes")
  x <- sf::st_coordinates(graph)[,"X"]
  y <- sf::st_coordinates(graph)[,"Y"]
  # Create layout data frame
  nodes <- data_frame0(x = x, y = y, circular = FALSE)
  # Convert to tbl_graph to 'unstick' geometry column
  extra_data <- as_tibble(as_tbl_graph(graph), active = "nodes")
  nodes <- combine_layout_nodes(nodes, extra_data)
  attr(nodes, 'graph') <- graph
  nodes
}
