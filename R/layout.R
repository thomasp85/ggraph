#' @rdname ggraph
#'
#' @aliases layout_ggraph
#'
#' @export
create_layout <- function(graph, layout, circular, ...) {
  if (is_layout(layout)) {
    cli::cli_warn(c(
      'Ignoring {.arg graph} as layout is already calculated',
      i = 'Pass the calculated layout to the {.arg graph} argument to silence this warning'
    ))
    return(layout)
  }
  UseMethod('create_layout', graph)
}
is_layout <- function(x) inherits(x, 'layout_ggraph')
#' @rdname ggraph
#' @export
create_layout.default <- function(graph, layout, ...) {
  graph <- try_fetch(as_tbl_graph(graph), error = function(e) {
    cli::cli_abort(
      'No layout function defined for objects of class {.cls {class(graph)[1]}}'
    )
  })
  create_layout(graph, layout, ...)
}
#' @rdname ggraph
#' @export
create_layout.layout_ggraph <- function(graph, ...) {
  graph
}
#' @export
as.data.frame.layout_ggraph <- function(x, ...) {
  extra_attr <- names(attributes(x))
  extra_attr <- extra_attr[!extra_attr %in% c('names', 'row.names')]
  attributes(x)[extra_attr] <- NULL
  class(x) <- 'data.frame'
  x
}
check_layout <- function(layout) {
  if (!is.data.frame(layout)) {
    cli::cli_abort('{.arg layout} must be a {.cls data.frame}')
  }
  if (!(is.numeric(layout$x) && is.numeric(layout$y))) {
    cli::cli_abort(
      '{.arg layout} must contain numeric {.col x} and {.col y} columns'
    )
  }
  graph <- attr(layout, 'graph')
  if (!is.tbl_graph(graph)) {
    cli::cli_abort(
      '{.arg layout} must have a {.cls tbl_graph} as the {.arg graph} attribute'
    )
  }
  if (nrow(layout) != gorder(graph)) {
    cli::cli_abort(
      '{.arg layout} must contain the same number of rows as the number of nodes in the graph'
    )
  }
  if (!'circular' %in% names(layout)) {
    layout$circular <- FALSE
  }
  if (!is.logical(layout$circular)) {
    cli::cli_abort('The {.col circular} column must be logical')
  }
  layout
}
