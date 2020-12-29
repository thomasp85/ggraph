#' Use igraph layout algorithms for layout_tbl_graph
#'
#' This layout function makes it easy to apply one of the layout algorithms
#' supplied in igraph when plotting with ggraph. Layout names are auto completed
#' so there is no need to write `layout_with_graphopt` or
#' `layout_as_tree`, just `graphopt` and `tree` (though the
#' former will also work if you want to be super explicit). Circular layout is
#' only supported for tree-like layout (`tree` and `sugiyama`) and
#' will throw an error when applied to other layouts.
#'
#' @details
#' igraph provides a huge amount of possible layouts. They are all briefly
#' described below:
#'
#' \strong{Hierarchical layouts}
#'
#' \describe{
#'   \item{`tree`}{Uses the *Reingold-Tilford* algorithm to place the
#'   nodes below their parent with the parent centered above its children. See
#'   [igraph::as_tree()]}
#'   \item{`sugiyama`}{Designed for directed acyclic graphs (that is,
#'   hierarchies where multiple parents are allowed) it minimizes the number of
#'   crossing edges. See [igraph::with_sugiyama()]}
#' }
#'
#' \strong{Standard layouts}
#'
#' \describe{
#'   \item{`bipartite`}{Minimize edge-crossings in a simple two-row (or
#'   column) layout for bipartite graphs. See [igraph::as_bipartite()]}
#'   \item{`star`}{Place one node in the center and the rest equidistantly
#'   around it. See [igraph::as_star()]}
#'   \item{`circle`}{Place nodes in a circle in the order of their index.
#'   Consider using [layout_tbl_graph_linear()] with `circular=TRUE`
#'   for more control. See [igraph::in_circle()]}
#'   \item{`nicely`}{Tries to pick an appropriate layout. See
#'   [igraph::nicely()] for a description of the simple decision tree
#'   it uses}
#'   \item{`dh`}{Uses *Davidson and Harels* simulated annealing
#'   algorithm to place nodes. See [igraph::with_dh()]}
#'   \item{`gem`}{Place nodes on the plane using the GEM force-directed
#'   layout algorithm. See [igraph::with_gem()]}
#'   \item{`graphopt`}{Uses the Graphopt algorithm based on alternating
#'   attraction and repulsion to place nodes. See
#'   [igraph::with_graphopt()]}
#'   \item{`grid`}{Place nodes on a rectangular grid. See
#'   [igraph::on_grid()]}
#'   \item{`mds`}{Perform a multidimensional scaling of nodes using either
#'   the shortest path or a user supplied distance. See
#'   [igraph::with_mds()]}
#'   \item{`sphere`}{Place nodes uniformly on a sphere - less relevant for
#'   2D visualizations of networks. See [igraph::on_sphere()]}
#'   \item{`randomly`}{Places nodes uniformly random. See
#'   [igraph::randomly()]}
#'   \item{`fr`}{Places nodes according to the force-directed algorithm of
#'   Fruchterman and Reingold. See [igraph::with_fr()]}
#'   \item{`kk`}{Uses the spring-based algorithm by Kamada and Kawai to
#'   place nodes. See [igraph::with_kk()]}
#'   \item{`drl`}{Uses the force directed algorithm from the DrL toolbox to
#'   place nodes. See [igraph::with_drl()]}
#'   \item{`lgl`}{Uses the algorithm from Large Graph Layout to place
#'   nodes. See [igraph::with_lgl()]}
#' }
#'
#' @note This function is not intended to be used directly but by setting
#' `layout = 'igraph'` in [create_layout()]
#'
#' @param graph A `tbl_graph` object.
#'
#' @param algorithm The type of layout algorithm to apply. See *Details* or
#' [igraph::layout_()] for links to the layouts supplied by igraph.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to `FALSE`. Only applicable to
#' `algorithm = 'tree'` and `algorithm = 'sugiyama'`.
#'
#' @param offset If `circular = TRUE`, where should it begin. Defaults to
#' `pi/2` which is equivalent to 12 o'clock.
#'
#' @param use.dummy Logical. In the case of `algorithm = 'sugiyama'` should the
#' dummy-infused graph be used rather than the original. Defaults to
#' `FALSE`.
#'
#' @param ... Arguments passed on to the respective layout functions
#'
#' @return A data.frame with the columns `x`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph graph_attr components layout_as_bipartite layout_as_star layout_as_tree layout_in_circle layout_nicely layout_with_dh layout_with_drl layout_with_gem layout_with_graphopt layout_on_grid layout_with_mds layout_with_sugiyama layout_on_sphere layout_randomly layout_with_fr layout_with_kk layout_with_lgl
#' @importFrom rlang quos eval_tidy
#' @importFrom stats setNames
#'
layout_tbl_graph_igraph <- function(graph, algorithm, circular, offset = pi / 2, use.dummy = FALSE, ...) {
  algorithm <- as.igraphlayout(algorithm)
  dots <- quos(...)
  dots <- setNames(lapply(names(dots), function(nq) {
    if (nq == 'weights') {
      eval_tidy(dots[[nq]], as_tibble(graph, active = 'edges'))
    } else {
      eval_tidy(dots[[nq]])
    }
  }), names(dots))
  layout <- do.call(algorithm, c(list(graph), dots))
  if (algorithm == 'layout_with_sugiyama') {
    if (use.dummy) {
      graph <- as_tbl_graph(layout$extd_graph)
      layout <- graph_attr(layout$extd_graph, 'layout')
    } else {
      layout <- layout$layout
    }
  }
  if (algorithm == 'layout_as_tree') {
    layout[, 1] <- layout[, 1] + components(graph)$membership - 1
  }
  if ('dim' %in% names(dots) && isTRUE(dots$dim > 2)) {
    warning('Only the first two dimensions will be used despite requesting more', call. = FALSE)
  }
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(data.frame(x = 1, y = 1), extra_data)
  layout <- cbind(x = layout[, 1], y = layout[, 2], extra_data[, !names(extra_data) %in% c('x', 'y'), drop = FALSE])
  graph <- add_direction(graph, layout)
  if (circular) {
    if (!algorithm %in% c('layout_as_tree', 'layout_with_sugiyama')) {
      stop('Circular layout only applicable to tree and DAG layout')
    }
    radial <- radial_trans(
      r.range = rev(range(layout$y)),
      a.range = range(layout$x),
      offset = offset
    )
    coords <- radial$transform(layout$y, layout$x)
    layout$x <- coords$x
    layout$y <- coords$y
  }
  layout$circular <- circular
  attr(layout, 'graph') <- graph
  layout
}
