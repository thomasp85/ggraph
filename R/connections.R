#' Create a connection extractor function
#'
#' Connections within the ggraph terminology are links between nodes that are
#' not part of the network structure itself. In that sense connections do not
#' affect the layout calculation in any way and will not be drawn by the
#' standard `geom_edge_*` functions. A connection does not need to only be
#' defined by a start and end node, but can include intermediary nodes.
#' `get_con` helps in creating connection data by letting you specify start
#' and end nodes and automatically finds the shortest path within the graph
#' structure that connects the given points. If this is not what is needed it is
#' also possible to supply a list of vectors giving node indices that define a
#' connection.
#'
#' @param from,to The index of the start and end nodes for the connections
#'
#' @param paths A list of integer vectors giving the index of nodes defining
#' connections
#'
#' @param ... Additional information to be added to the final data output.
#' Accepts expressions that will be evaluated on the node data in it's
#' original order (irrespective of any reordering by the layout)
#'
#' @param weight An expression to be evaluated on the edge data to provide
#' weights for the shortest path calculations
#'
#' @inheritParams igraph::shortest_paths
#'
#' @return A function that takes a layout_ggraph object and returns the given
#' connections
#'
#' @family extractors
#'
#' @export
get_con <- function(from = integer(), to = integer(), paths = NULL, ..., weight = NULL, mode = 'all') {
  if (length(from) != length(to)) {
    cli::cli_abort('{.arg from} and {.arg to} must be of equal length')
  }
  dots <- enquos(...)
  function(layout) {
    if (length(from) == 0) {
      return(NULL)
    }
    connections <- collect_connections(
      layout = layout, from = from, to = to,
      weight = {
        {
          weight
        }
      }, mode = mode
    )
    nodes <- data_frame0(layout)[unlist(connections), ]
    nodes$con.id <- rep(seq_along(connections), lengths(connections))
    if (!is.null(paths)) {
      extra <- data_frame0(layout)[unlist(paths), ]
      extra$con.id <- rep(
        seq_along(paths) + length(connections),
        lengths(paths)
      )
      nodes <- vec_rbind(nodes, extra)
    }
    layout <- layout[order(layout$.ggraph.orig_index), ]
    extra_data <- lapply(dots, function(x) {
      val <- eval_tidy(x, layout)
      rep(val, length.out = length(from))[nodes$con.id]
    })
    if (length(extra_data) > 0) {
      nodes <- cbind(
        nodes,
        data_frame0(!!!extra_data)
      )
    }
    attr(nodes, 'type') <- 'connection_ggraph'
    nodes
  }
}
#' Internal data extractors
#'
#' These functions exists for supporting different data structures. There is no
#' need to call these directly
#'
#' @param layout The layout data
#'
#' @param from,to A numeric vector giving the indexes of the start and end nodes
#'
#' @param ... Additional parameters passed on to the specific method
#'
#' @keywords internal
#' @export
#' @rdname internal_extractors
#' @name internal_extractors
collect_connections <- function(layout, from, to, ...) {
  UseMethod('collect_connections', layout)
}
#' @export
collect_connections.default <- function(layout, ...) {
  cli::cli_abort('Don\'t know how to get connections from an object of class {.cls {class(layout)[1]}}')
}
