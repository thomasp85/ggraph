#' Place nodes according to the standard design of metro maps
#'
#' This layouttries to optimise the placement of nodes so that they adhere to
#' the classic look of metro maps. As such it optimizes on the distribution of
#' incident edge angles, conformity of edge length, and edge angles in multiples
#' of 45 degrees. As it works as a refinement of an already existing layout
#' (the physical location of metro stations) it requires an a priori node
#' location. Due to it's purpose it probably works best with planar graphs.
#'
#' @param graph A tbl_graph object
#' @param x,y The start location of the nodes
#' @param length Desired multiple of grid point spacing. (`length * grid_space`
#' determines desired edge length)
#' @param grid_space The distance between consecitive grid points
#' @param optimization_weights The relative weight to be placed on the 5
#' criteria during optimization as a numeric vector of length 4. The criteria
#' are:
#' * `Angular Resolution Criterion`: The angles of incident edges at each
#'   station should be maximized, because if there is only a small angle between
#'   any two adjacent edges, then it can become difficult to distinguish between
#'   them.
#' * `Edge Length Criterion`: The edge lengths across the whole map should be
#'   approximately equal to ensure regular spacing between stations. It is based
#'   on the preferred multiple, l, of the grid spacing, g. The purpose of the
#'   criterion is to penalize edges that are longer than or shorter than lg.
#' * `Balanced Edge Length Criterion`: The length of edges incident to a
#'   particular station should be similar.
#' * `Line Straightness Criterion`: (not yet implemented) Edges that form part
#'   of a line should, where possible, be co-linear either side of each station
#'   that the line passes through.
#' * `Octiinearity Criterion`: Each edge should be drawn horizontally,
#'   vertically, or diagonally at 45 degree, so we penalize edges that are not
#'   at a desired angle.
#' If `NULL` all criteria are given equal weight.
#' @param max_movement Number of grid points a station can move away rom its
#' original position
#' @param circular ignored
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @references
#' Stott, J., Rodgers, P., Martinez-Ovando, J. C., and Walker, S. G. (2011).
#' *Automatic metro map layout using multicriteria optimization* In IEEE Trans
#' Vis Comput Graph 17(1) pp. 101-114. https://doi.org/10.1109/tvcg.2010.24
#'
#' @family layout_tbl_graph_*
#'
#' @author The underlying algorithm is implemented in the graphlayouts package
#' by David Schoch
#'
#' @importFrom graphlayouts layout_as_metromap
#'
layout_tbl_graph_metro <- function(
  graph,
  x,
  y,
  length = 2,
  grid_space = 0.0025,
  optimization_weights = NULL,
  max_movement = 5,
  circular = FALSE
) {
  x <- eval_tidy(enquo(x), .N())
  y <- eval_tidy(enquo(y), .N())
  if (is.null(x) || is.null(y)) {
    cli::cli_abort(
      "The metro layout must have a prior location of nodes to start with"
    )
  }
  if (is.null(optimization_weights)) {
    optimization_weights <- rep(1, 5)
  }
  xy <- layout_as_metromap(
    graph,
    xy = cbind(x, y),
    l = length,
    gr = grid_space,
    w = optimization_weights,
    bsize = max_movement
  )
  nodes <- data_frame0(x = xy[, 1], y = xy[, 2], circular = FALSE)
  combine_layout_nodes(nodes, as_tibble(graph, active = 'nodes'))
}
