#' Place nodes on a line or circle
#'
#' This layout puts all nodes on a line, possibly sorted by a node attribute. If
#' `circular = TRUE` the nodes will be laid out on the unit circle instead.
#' In the case where the `sort.by` attribute is numeric, the numeric values
#' will be used as the x-position and it is thus possible to have uneven spacing
#' between the nodes.
#'
#' @param graph An `tbl_graph` object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to `FALSE`.
#'
#' @param sort.by The name of a node variable to sort the nodes by.
#'
#' @param use.numeric Logical. Should a numeric sort.by attribute be used as the
#' actual x-coordinates in the layout. May lead to overlapping nodes. Defaults
#' to FALSE
#'
#' @param offset If `circular = TRUE`, where should it begin. Defaults to
#' `pi/2` which is equivalent to 12 o'clock.
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder
#'
layout_tbl_graph_linear <- function(graph, circular, sort.by = NULL, use.numeric = FALSE, offset = pi / 2) {
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  if (!is.null(sort.by)) {
    if (is.numeric(sort.by) && use.numeric) {
      x <- sort.by
    } else {
      x <- order(order(sort.by))
    }
  } else {
    x <- seq_len(gorder(graph))
  }
  nodes <- new_data_frame(list(x = x, y = 0))
  if (circular) {
    radial <- radial_trans(
      r.range = rev(range(nodes$y)),
      a.range = range(nodes$x),
      offset = offset
    )
    coords <- radial$transform(nodes$y, nodes$x)
    nodes$x <- coords$x
    nodes$y <- coords$y
  }
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes$circular <- circular
  nodes
}
