#' Create edge extractor function
#'
#' This function returns another function that can extract edges from a
#' ggraph_layout object. The functionality of the returned function is decided
#' by the arguments to `get_edges`. The need for `get_edges` is mainly to
#' pass to the `data` argument of the different `geom_edge_*`
#' functions in order to present them with the right kind of data. In general
#' each `geom_edge_*` has the default set correctly so there is only need
#' to modify the data argument if parallel edges should be collapsed.
#'
#' @details
#' There are two types of return formats possible for the result of the returned
#' function:
#'
#' \describe{
#'   \item{short}{In this format each edge is described in one line in the
#'   format expected for [ggplot2::geom_segment()], that is, the start
#'   node position is encoded in the `x` and `y` column and the end
#'   node position is encoded in the `xend` and `yend` column. If node
#'   parameters are added to the edge the name of the parameters will be
#'   prefixed with `node1.` for the start node and `node2.` for the
#'   end node.}
#'   \item{long}{In this format each edge consists of two rows with matching
#'   `edge.id` value. The start and end position are both encoded in the
#'   `x` and `y` column. The relative position of the rows determines
#'   which is the start and end node, the first occurring being the start node.
#'   If node parameters are added to the edge data the name of the parameters
#'   will be prefixed with `node.`.}
#' }
#'
#' Node parameters are automatically added so it is possible to format edge
#' aesthetics according to start or end node parameters, or interpolate edge
#' aesthetics between start and end node parameters. Node parameters will be
#' prefixed to avoid name clash with edge parameters. The prefix depends on the
#' format (see above).
#'
#' If the graph is not simple (it contains at most one edge between each node
#' pair) it can be collapsed so either all edges between two nodes or all edges
#' of the same direction between two nodes are merged. The edge parameters are
#' taken from the first occurring edge, so if some more sophisticated summary is
#' needed it is suggested that the graph be tidied up before plotting with
#' ggraph.
#'
#' @param format Either `'short'` (the default) or `'long'`. See
#' details for a descriptions of the differences
#'
#' @param collapse Either `'none'` (the default), `'all'` or
#' `'direction'`. Specifies whether parallel edges should be merged. See
#' details for more information
#'
#' @param ... Additional data that will be cbind'ed together with the returned
#' edge data. Accepts expressions that will be evaluated on the edge data
#'
#' @return A data.frame with columns dependent on format as well as the graph
#' type. In addition to the columns discussed in the details section,
#' the data.frame will always contain the columns `from`, `to` and
#' `circular`, the two former giving the indexes of the start and end node
#'  and the latter if the layout is circular (needed for correct formatting of
#'  some `geom_edge_*`). The graph dependent information is:
#'
#' \describe{
#'   \item{dendrogram}{A `label` column will hold the value of the
#'   `edgetext` attribute. In addition any value stored in the
#'   `edgePar` attribute will be added. Lastly a `direction` column
#'   will hold the relative position between the start and end nodes (needed for
#'   correct formatting of [geom_edge_elbow()]).}
#'   \item{igraph}{All edge attributes of the original graph object is added as
#'   columns to the data.frame}
#' }
#'
#' @family extractors
#'
#' @export
#'
get_edges <- function(format = 'short', collapse = 'none', ...) {
  if (!collapse %in% c('none', 'all', 'direction')) {
    cli::cli_abort('{.arg collapse} must be either {.val none}, {.val all} or {.val direction}')
  }
  dots <- enquos(...)
  function(layout) {
    edges <- collect_edges(layout)
    edges <- switch(
      collapse,
      none = edges,
      all = collapse_all_edges(edges),
      direction = collapse_dir_edges(edges)
    )
    edges <- switch(
      format,
      short = format_short_edges(edges, layout),
      long = format_long_edges(edges, layout),
      cli::cli_abort('Unknown {.arg format}. Use either {.val short} or {.val long}')
    )
    extra_data <- lapply(dots, function(x) {
      val <- eval_tidy(x, edges)
      rep(val, length.out = nrow(edges))
    })
    if (length(extra_data) > 0) {
      edges <- cbind(
        edges,
        data_frame0(!!!extra_data)
      )
    }
    attr(edges, 'type_ggraph') <- 'edge_ggraph'
    edges
  }
}
#' @rdname internal_extractors
#' @export
collect_edges <- function(layout) {
  UseMethod('collect_edges', layout)
}
#' @export
collect_edges.default <- function(layout) {
  attr(layout, 'edges')
}
check_short_edges <- function(edges) {
  if (!inherits(edges, 'data.frame')) {
    cli::cli_abort('{.arg edges} must by of class {.cls data.frame}')
  }
  if (!all(c('from', 'to', 'x', 'y', 'xend', 'yend', 'circular', 'edge.id') %in% names(edges))) {
    cli::cli_abort('{.arg edges} must contain the columns {.and {.col {c("from", "to", "x", "y", "xend", "yend", "circular", "edge.id")}}}')
  }
  if (!is.logical(edges$circular)) {
    cli::cli_abort('The {.col circular} column must be logical')
  }
  edges
}
check_long_edges <- function(edges) {
  if (!inherits(edges, 'data.frame')) {
    cli::cli_abort('{.arg edges} must by of class {.cls data.frame}')
  }
  if (!all(c('edge.id', 'node', 'x', 'y', 'circular') %in% names(edges))) {
    cli::cli_abort('{.arg edges} must contain the columns {.and {.col {c("edge.id", "node", "x", "y", "circular")}}}')
  }
  if (!all(range(table(edges$edge.id)) == 2)) {
    cli::cli_abort('Each edge must consist of two rows')
  }
  if (!is.logical(edges$circular)) {
    cli::cli_abort('The {.col circular} column must be logical')
  }
  edges
}
add_edge_coordinates <- function(edges, layout) {
  edges$x <- layout$x[edges$from]
  edges$y <- layout$y[edges$from]
  edges$xend <- layout$x[edges$to]
  edges$yend <- layout$y[edges$to]
  edges
}
format_short_edges <- function(edges, layout) {
  edges <- add_edge_coordinates(edges, layout)
  nodes1 <- layout[edges$from, , drop = FALSE]
  names(nodes1) <- paste0('node1.', names(nodes1))
  nodes2 <- layout[edges$to, , drop = FALSE]
  names(nodes2) <- paste0('node2.', names(nodes2))
  edges <- cbind(edges, nodes1, nodes2)
  rownames(edges) <- NULL
  edges$edge.id <- seq_len(nrow(edges))
  check_short_edges(edges)
}
format_long_edges <- function(edges, layout) {
  from <- cbind(
    edge.id = seq_len(nrow(edges)),
    node = edges$from,
    layout[edges$from, c('x', 'y')],
    edges
  )
  to <- cbind(
    edge.id = seq_len(nrow(edges)),
    node = edges$to,
    layout[edges$to, c('x', 'y')],
    edges
  )
  edges <- vec_rbind(from, to)
  node <- layout[edges$node, , drop = FALSE]
  names(node) <- paste0('node.', names(node))
  edges <- cbind(edges, node)
  rownames(edges) <- NULL
  check_long_edges(edges[order(edges$edge.id), ])
}
complete_edge_aes <- function(aesthetics) {
  if (is.null(aesthetics)) {
    return(aesthetics)
  }
  if (any(names(aesthetics) == 'color')) {
    names(aesthetics)[names(aesthetics) == 'color'] <- 'colour'
  }
  expand_edge_aes(aesthetics)
}
expand_edge_aes <- function(x) {
  short_names <- names(x) %in% c(
    'colour', 'color', 'fill', 'linetype', 'shape', 'size', 'width', 'alpha', 'linewidth'
  )
  names(x)[short_names] <- paste0('edge_', names(x)[short_names])
  if (all(c('edge_linewidth', 'edge_width') %in% names(x) == c(TRUE, FALSE))) {
    names(x)[names(x) == 'edge_linewidth'] <- 'edge_width'
  }
  x
}
#' @importFrom dplyr group_by top_n ungroup
collapse_all_edges <- function(edges) {
  from <- pmin(edges$from, edges$to)
  to <- pmax(edges$to, edges$from)
  id <- paste(from, to, sep = '-')
  if (anyDuplicated(id)) {
    edges$.id <- id
    edges <- edges %>%
      group_by(.data$.id) %>%
      top_n(1) %>%
      ungroup()
  }
  data_frame0(edges)
}
#' @importFrom dplyr group_by top_n ungroup
collapse_dir_edges <- function(edges) {
  id <- paste(edges$from, edges$to, sep = '-')
  if (anyDuplicated(id)) {
    edges$.id <- id
    edges <- edges %>%
      group_by(.data$.id) %>%
      top_n(1) %>%
      ungroup()
  }
  data_frame0(edges)
}
