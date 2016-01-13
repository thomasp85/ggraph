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
#' @export
getEdges <- function(layout) {
    UseMethod('getEdges', layout)
}
#' @export
getEdges.default <- function(layout) {
    checkEdges(attr(layout, 'edges'))
}
checkEdges <- function(edges) {
    if (!inherits(edges, 'data.frame')) {
        stop('edges must by of class data.frame', call. = FALSE)
    }
    if (!all(c('from', 'to', 'x', 'y', 'xend', 'yend', 'circular') %in% names(edges))) {
        stop('edges must contain the columns from, to, x, y, xend, yend and circular', call. = FALSE)
    }
    if (!is.logical(edges$circular)) {
        stop('circular column must be logical', call. = FALSE)
    }
    edges
}
addEdgeCoordinates <- function(edges, layout) {
    edges$x <- layout$x[edges$from]
    edges$y <- layout$y[edges$from]
    edges$xend <- layout$x[edges$to]
    edges$yend <- layout$y[edges$to]
    edges
}
