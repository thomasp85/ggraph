#' Place nodes in circles based on distance to a specific node
#'
#' This layout constrains node placement to a radius relative to its distance to
#' a given node. It then uses stress majorisation to find an optimal node
#' distribution according to this constraint.
#'
#' @param focus An expression evaluating to a selected node. Can either be a
#' single integer or a logical vector with a single `TRUE` element.
#' @inheritParams layout_tbl_graph_stress
#'
#' @return A data.frame with the columns `x`, `y`, `circular`, `distance` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @references
#' Brandes, U., & Pich, C. (2011). *More flexible radial layout.* Journal of
#' Graph Algorithms and Applications, 15(1), 157-173.
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_with_focus
#' @importFrom rlang eval_tidy enquo
layout_tbl_graph_focus <- function(graph, focus, weights = NULL, niter = 500, tolerance = 1e-4,
                                   circular = TRUE) {
  focus <- eval_tidy(enquo(focus), .N())
  if (is.logical(focus)) focus <- which(focus)[1]

  weights <- eval_tidy(enquo(weights), .E())
  if (is.null(weights)) {
    weights <- NA
  }

  layout <- layout_with_focus(graph, v = focus, weights = weights, iter = niter,
                              tol = tolerance)
  xy <- layout$xy

  nodes <- new_data_frame(list(x = xy[,1],y = xy[,2], distance = layout$distance))
  nodes$circular <- FALSE
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes
}
