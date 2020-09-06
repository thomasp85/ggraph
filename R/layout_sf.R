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
#'
#' @importFrom sf st_coordinates
layout_tbl_graph_sf <- function(graph, circular = FALSE) {

  graph = activate(graph, "nodes")

  x <- st_coordinates(graph)[,"X"]
  y <- st_coordinates(graph)[,"Y"]

  layout <- new_data_frame(list(x = x, y = y))

  layout$circular <- FALSE

  attr(layout, 'graph') <- graph
  layout
}
