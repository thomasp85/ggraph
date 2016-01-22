#' @export
createLayout <- function(graph, layout, ...) {
    UseMethod('createLayout', graph)
}
#' @export
createLayout.default <- function(graph, layout, ...) {
    stop('No layout function defined for objects of class ', class(graph))
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
