#' Automatically pick a layout based on graph type
#'
#' This function infers the layout from the graph structure and is the default
#' when calling [ggraph()]. If an `x` and `y` argument is passed along, the
#' manual layout is chosen. Otherwise if the graph is either a rooted tree or a
#' rooted forest the layout will be `dendrogram` if the nodes contains a height
#' variable or `tree` if not. If the tree is unrooted the `unrooted` layout will
#' be used. If the tree is a DAG the `sygiyama` layout will be used. Otherwise
#' the `stress` layout will be used (or `sparse_tree` if the graph contains more
#' than 2000 nodes).
#'
#' @param graph A tbl_graph object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to `FALSE`. Only applicable if the graph is a tree
#' structure
#'
#' @param ... Arguments passed on to the chosen layout
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom rlang .data quos
#'
layout_tbl_graph_auto <- function(graph, circular, ...) {
  if (all(c('x', 'y') %in% names(quos(...)))) {
    layout_tbl_graph_manual(graph, circular = circular, ...)
  } else if (with_graph(graph, graph_is_tree() || graph_is_forest())) {
    if (is.null(with_graph(graph, .N()[['height']]))) {
      message('Using `tree` as default layout')
      layout_tbl_graph_igraph(graph, algorithm = 'tree', circular = circular, ...)
    } else {
      message('Using `dendrogram` as default layout')
      layout_tbl_graph_dendrogram(graph, circular = circular, height = .data$height, ...)
    }
  } else if (with_graph(graph, graph_is_dag())) {
    message('Using `sugiyama` as default layout')
    layout_tbl_graph_igraph(graph, algorithm = 'sugiyama', circular = circular, ...)
  } else if (with_graph(graph, graph_order()) < 2000) {
    message('Using `stress` as default layout')
    layout_tbl_graph_stress(graph, circular = circular, ...)
  } else {
    message('Using `sparse_stress` with 250 pivots as default layout')
    layout_tbl_graph_sparse_stress(graph, pivots = 250, circular = circular, ...)
  }
}
