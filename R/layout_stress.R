#' Place nodes using stress majorisation
#'
#' This layout is related to the stress-minimization algorithm known as
#' Kamada-Kawai (available as the 'kk' layout), but uses another optimization
#' strategy. It generally have better runtime, quality, and stability compared
#' to the Kamada-Kawai layout and is thus generally preferred. The sparse
#' version of the layout have better performance (especially on larger networks)
#' at the expense of layout quality, but will generally outperform many other
#' algorithms for large graphs in both runtime and quality (e.g. the 'drl'
#' layout from igraph).
#'
#' @param graph a tbl_graph object
#' @param weights An expression evaluated on the edge data to provide edge
#' weights for the layout. Currently ignored for the sparse version
#' @param pivots The number of pivot nodes.
#' @param niter number of iterations during stress optimization
#' @param tolerance stopping criterion for stress optimization
#' @param mds should an MDS layout be used as initial layout (default: TRUE)
#' @param bbox constrain dimension of output. Only relevant to determine the
#' placement of disconnected graphs.
#' @param x,y Expressions evaluated on the node data giving
#' coordinates along x and/or y axis to fix nodes to. You can chose to only fix
#' selected nodes by leaving the remaining nodes with `NA` values.
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @references
#' Gansner, E. R., Koren, Y., & North, S. (2004). *Graph drawing by stress
#' majorization.* In International Symposium on Graph Drawing (pp. 239-250). Springer, Berlin, Heidelberg.
#'
#' Ortmann, M. and Klimenta, M. and Brandes, U. (2016). *A Sparse Stress Model.* https://arxiv.org/pdf/1608.08909.pdf
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_with_stress layout_with_constrained_stress layout_with_fixed_coords
#' @importFrom igraph gorder
#' @importFrom rlang eval_tidy enquo
#'
layout_tbl_graph_stress <- function(
  graph,
  weights = NULL,
  niter = 500,
  tolerance = 1e-4,
  mds = TRUE,
  bbox = 50,
  x = NULL,
  y = NULL,
  circular = FALSE
) {
  weights <- eval_tidy(enquo(weights), .E())
  if (is.null(weights)) {
    weights <- NA
  } else {
    weights <- 1 / weights
  }
  x <- eval_tidy(enquo(x), .N())
  y <- eval_tidy(enquo(y), .N())
  if (is.null(x) && is.null(y)) {
    xy <- layout_with_stress(
      graph,
      weights = weights,
      iter = niter,
      tol = tolerance,
      mds = mds,
      bbox = bbox
    )
  } else {
    xy <- cbind(x %||% NA, y %||% NA)
    if (nrow(xy) != gorder(graph)) {
      cli::cli_abort(
        "If {.arg x_coord} and/or {.arg y_coord} is given they must equal the number of nodes in the graph"
      )
    }
    if (anyNA(xy)) {
      xy <- layout_with_fixed_coords(
        graph,
        xy,
        weights = weights,
        iter = niter,
        tol = tolerance,
        mds = mds,
        bbox = bbox
      )
    }
  }

  nodes <- data_frame0(x = xy[, 1], y = xy[, 2], circular = FALSE)
  combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
}
#' @rdname layout_tbl_graph_stress
#' @importFrom graphlayouts layout_with_sparse_stress
layout_tbl_graph_sparse_stress <- function(
  graph,
  pivots,
  weights = NULL,
  niter = 500,
  circular = FALSE
) {
  weights <- eval_tidy(enquo(weights))
  if (!is.null(weights)) {
    cli::cli_warn(
      '{.arg weights} is currently ignored for sparse stress layouts'
    )
  }
  xy <- layout_with_sparse_stress(
    graph,
    pivots = pivots,
    weights = weights,
    iter = niter
  )
  nodes <- data_frame0(x = xy[, 1], y = xy[, 2], circular = FALSE)
  combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
}
