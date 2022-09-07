#' Manually specify a layout for layout_tbl_graph
#'
#' This layout function lets you pass the node positions in manually. The
#' supplied positions must match the order of the nodes in the tbl_graph
#'
#' @param graph An `tbl_graph` object
#'
#' @param x,y Expressions with the x and y positions of the nodes
#'
#' @param circular Ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom rlang enquo eval_tidy quo_is_symbol
#'
layout_tbl_graph_manual <- function(graph, x, y, circular) {
  if (isTRUE(circular)) {
    cli::cli_warn('{.arg circular} argument ignored for manual layout')
  }
  x <- enquo(x)
  y <- enquo(y)
  nodes <- data_frame0(
    x = eval_tidy(x, .N()),
    y = eval_tidy(y, .N()),
    circular = FALSE
  )
  nodes <- combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
  nodes
}
