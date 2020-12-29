#' @rdname ggraph
#'
#' @aliases layout_ggraph
#'
#' @export
create_layout <- function(graph, layout, circular, ...) {
  if (is_layout(layout)) {
    warning('Ignoring `graph` as layout is already calculated. Pass the calculated layout to the `graph` argument to silence this warning', call. = FALSE)
    return(layout)
  }
  UseMethod('create_layout', graph)
}
is_layout <- function(x) inherits(x, 'layout_ggraph')
#' @rdname ggraph
#' @export
create_layout.default <- function(graph, layout, ...) {
  graph <- tryCatch(as_tbl_graph(graph), error = function(e) {
    stop('No layout function defined for objects of class ', class(graph), call. = FALSE)
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
    stop('layout must be a data.frame', call. = FALSE)
  }
  if (!(is.numeric(layout$x) && is.numeric(layout$y))) {
    stop('layout must contain numeric `x` and `y` columns', call. = FALSE)
  }
  graph <- attr(layout, 'graph')
  if (!is.tbl_graph(graph)) {
    stop('layout must have a tbl_graph as the `graph` attribute', call. = FALSE)
  }
  if (nrow(layout) != gorder(graph)) {
    stop('layout must contain the same number of rows as nodes', call. = FALSE)
  }
  if (!'circular' %in% names(layout)) {
    layout$circular <- FALSE
  }
  if (!is.logical(layout$circular)) {
    stop('circular column must be logical', call. = FALSE)
  }
  layout
}
