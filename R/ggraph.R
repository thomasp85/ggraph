#' Create a ggraph plot
#'
#' This function is the equivalent of \code{\link[ggplot2]{ggplot}} in ggplot2.
#' It takes care of setting up the plot object along with creating the layout
#' for the plot based on the graph and the specification passed in.
#' Alternatively a layout can be prepared in advance using
#' \code{\link{createLayout}} and passed as the data argument.
#'
#' @param graph The object containing the graph. See \code{\link{createLayout}}
#' for a list of supported classes.
#'
#' @param layout The type of layout to create.
#'
#' @param data A layout_ggraph object. If supplied \code{graph} and
#' \code{layout} are ignored.
#'
#' @param ... Arguments passed on to \code{\link{createLayout}}.
#'
#' @return An object of class gg onto which layers and scales can be added.
#'
#' @export
#'
ggraph <- function(graph, layout = 'auto', data, ...) {
    envir <- parent.frame()
    if (missing(data)) {
        layout <- createLayout(graph, layout, ...)
    } else {
        if (inherits(data, 'layout_ggraph')) {
            layout <- data
        } else {
            stop('Supplied data must inherit from layout_ggraph')
        }
    }
    ggplot(data = layout, environment = envir)
}
