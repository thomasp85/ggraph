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
  if (circular) {
    warning('circular argument ignored for manual layout')
  }
  x <- enquo(x)
  y <- enquo(y)
  layout <- new_data_frame(list(
    x = eval_tidy(x, .N()),
    y = eval_tidy(y, .N())
  ))
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(layout, extra_data)
  layout <- cbind(layout, extra_data[, !names(extra_data) %in% names(layout), drop = FALSE])
  layout$circular <- FALSE
  layout
}
