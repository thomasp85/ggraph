#' @rdname ggraph
#'
#' @aliases layout_ggraph
#'
#' @export
create_layout <- function(graph, layout, circular, ...) {
    UseMethod('create_layout', graph)
}
#' @rdname ggraph
#' @usage NULL
#' @export
createLayout <- function(...) {
    .Deprecated('create_layout')
    create_layout(...)
}
#' @rdname ggraph
#' @export
create_layout.default <- function(graph, layout, ...) {
    stop('No layout function defined for objects of class ', class(graph))
}
#' @export
as.data.frame.layout_ggraph <- function(x, ...) {
    extraAttr <- names(attributes(x))
    extraAttr <- extraAttr[!extraAttr %in% c('names', 'row.names')]
    attributes(x)[extraAttr] <- NULL
    class(x) <- 'data.frame'
    x
}
checkLayout <- function(layout) {
    if (!inherits(layout, 'data.frame')) {
        stop('layout must subclass data.frame', call. = FALSE)
    }
    if (!all(c('x', 'y', 'circular') %in% names(layout))) {
        stop('layout must contain the columns x, y and circular', call. = FALSE)
    }
    if (!is.logical(layout$circular)) {
        stop('circular column must be logical', call. = FALSE)
    }
    layout
}
