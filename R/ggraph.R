#' @importFrom ggplot2 ggplot
#' @export
ggraph <- function(graph, layout, data, ...) {
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
