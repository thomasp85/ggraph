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
#' @param weight A weight for each node. Nodes will be spread out according to
#' their weight so that nodes with heigher weight will have more space around
#' them. Ignored if `use.numeric = TRUE`
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#' Further, if `circular = FALSE` a `width` column and if `circular = TRUE` a
#' `start`, `end`, and `r0` column.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder
#'
layout_tbl_graph_linear <- function(
  graph,
  circular,
  sort.by = NULL,
  use.numeric = FALSE,
  offset = pi / 2,
  weight = NULL
) {
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  weight <- enquo(weight)
  weight <- eval_tidy(weight, .N())

  if (use.numeric && !is.null(weight)) {
    cli::cli_warn('Ignoring {.arg weight} when {.code use.numeric = TRUE}')
  }
  weight <- weight %||% rep(1, gorder(graph))

  if (length(weight) != gorder(graph)) {
    cli::cli_abort('{.arg weight} must match the order of the graph')
  }
  if (any(weight < 0)) {
    cli::cli_abort("{.arg weight} must be positive")
  }

  if (!is.null(sort.by)) {
    if (length(sort.by) != gorder(graph)) {
      cli::cli_abort('{.arg sort.by} must match the order of the graph')
    }
    if (is.numeric(sort.by) && use.numeric) {
      x <- sort.by
      weight <- 0
    } else {
      x <- c(0, cumsum(weight[order(sort.by)]))
      x <- (x[-1] + x[-length(x)]) / 2
      x <- x[order(order(sort.by))]
    }
  } else {
    x <- c(0, cumsum(weight))
    x <- (x[-1] + x[-length(x)]) / 2
  }
  nodes <- data_frame0(x = x, y = 0, width = weight)
  if (circular) {
    full_width <- sum(nodes$width)
    if (full_width > 0) {
      nodes$start <- 2 * pi * (nodes$x - nodes$width / 2) / full_width
      nodes$end <- 2 * pi * (nodes$x + nodes$width / 2) / full_width
    } else {
      full_width <- max(nodes$x)
      nodes$start <- 2 * pi * nodes$x / full_width
      nodes$end <- nodes$start
    }
    mid <- (nodes$start + nodes$end) / 2
    nodes$x <- sin(mid)
    nodes$y <- cos(mid)
    nodes$r0 <- 1
  }
  nodes <- combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
  nodes$circular <- circular
  nodes
}
