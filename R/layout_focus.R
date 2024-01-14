#' Place nodes in circles based on distance to a specific node
#'
#' This layout constrains node placement to a radius relative to its distance to
#' a given node. It then uses stress majorisation to find an optimal node
#' distribution according to this constraint.
#'
#' @param focus An expression evaluating to a selected node. Can either be a
#' single integer or a logical vector with a single `TRUE` element.
#' @inheritParams layout_tbl_graph_stress
#' @inheritParams layout_tbl_graph_centrality
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
#' @importFrom graphlayouts layout_with_focus layout_with_focus_group
#' @importFrom rlang eval_tidy enquo
layout_tbl_graph_focus <- function(graph, focus, weights = NULL, niter = 500, tolerance = 1e-4,
                                   group = NULL, shrink = 10, circular = TRUE) {
  focus <- eval_tidy(enquo(focus), .N())
  if (is.logical(focus)) focus <- which(focus)[1]

  weights <- eval_tidy(enquo(weights), .E())
  if (is.null(weights)) {
    weights <- NA
  }

  group <- eval_tidy(enquo(group), .N())

  if (is.null(group)) {
    layout <- layout_with_focus(graph, v = focus, weights = weights, iter = niter,
                                tol = tolerance)
  } else {
    layout <- layout_with_focus_group(graph, v = focus, group = group,
                                      shrink = shrink, weights = weights, iter = niter,
                                      tol = tolerance)
  }

  xy <- layout$xy

  nodes <- data_frame0(x = xy[,1],y = xy[,2], distance = layout$distance, circular = FALSE)
  combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
}
