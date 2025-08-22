#' Place nodes in circles according to centrality measure
#'
#' This layout places nodes in circles with the radii relative to a given
#' centrality measure. Under the hood it use stress majorisation to place nodes
#' optimally given the radius constraint.
#'
#' @param graph A tbl_graph object
#' @param centrality An expression evaluating to a centrality measure for the
#' nodes. See the different `centrality_*()` algorithms in tidygraph for a
#' selection.
#' @param scale Should the centrality measure be scaled between 0 and 100
#' @param tseq Transitioning steps
#' @param group An expression evaluating to a grouping of the nodes. If given
#' the layout will keep grouped nodes within an angle range of the origin
#' @param shrink shrink the reserved angle range for a group to increase the
#' gaps between groups
#' @inheritParams layout_tbl_graph_stress
#'
#' @return A data.frame with the columns `x`, `y`, `circular`, `centrality` as
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
#' @importFrom graphlayouts layout_with_centrality layout_with_centrality_group
#' @importFrom rlang eval_tidy enquo
layout_tbl_graph_centrality <- function(
  graph,
  centrality,
  scale = TRUE,
  niter = 500,
  tolerance = 1e-4,
  tseq = seq(0, 1, 0.2),
  group = NULL,
  shrink = 10,
  circular = FALSE
) {
  centrality <- eval_tidy(enquo(centrality), .N())
  group <- eval_tidy(enquo(group), .N())
  if (is.null(group)) {
    xy <- layout_with_centrality(
      graph,
      cent = centrality,
      scale = scale,
      iter = niter,
      tol = tolerance,
      tseq = tseq
    )
  } else {
    xy <- layout_with_centrality_group(
      graph,
      cent = centrality,
      group = group,
      shrink = shrink,
      scale = scale,
      iter = niter,
      tol = tolerance,
      tseq = tseq
    )
  }

  nodes <- data_frame0(
    x = xy[, 1],
    y = xy[, 2],
    centrality = centrality,
    group = group,
    circular = FALSE
  )
  combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
}
