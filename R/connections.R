#' Create a connection extractor function
#'
#' Connections within the ggraph terminology are links between nodes that are
#' not part of the network structure itself. In that sense connections does not
#' affect the layout calculation in any way and will not be drawn by the
#' standard \code{geom_edge_*} functions. A connection does not need to only be
#' defined by a start and end node, but can include intermediary nodes.
#' \code{get_con} helps in creating connection data by letting you specify start
#' and end node and automatically find the shortest path within the graph
#' structure that connects the given points. If this is not what is needed it is
#' also possible to supply a list of vectors giving node indexes that defines a
#' connection.
#'
#' @param from,to The index of the start and end nodes for the connections
#'
#' @param paths A list of integer vectors giving the index of nodes defining
#' connections
#'
#' @param ... Additional information to be added to the final data output
#'
#' @return A function that takes a layout_ggraph object and returns the given
#' connections
#'
#' @family extractors
#'
#' @export
get_con <- function(from = integer(), to = integer(), paths = NULL, ...) {
    if (length(from) != length(to)) {
        stop('from and to must be of equal length')
    }
    function(layout) {
        if (length(from) == 0) return(NULL)
        connections <- getConnections(layout, from, to)
        nodes <- as.data.frame(layout)[unlist(connections), ]
        nodes$con.id <- rep(seq_along(connections), lengths(connections))
        if (!is.null(paths)) {
            extra <- as.data.frame(layout)[unlist(paths), ]
            extra$con.id <- rep(seq_along(paths) + length(connections),
                                lengths(paths))
            nodes <- rbind(nodes, extra)
        }
        nodes <- do.call(
            cbind,
            c(list(nodes),
              lapply(list(...), rep, length.out = nrow(nodes)),
              list(stringsAsFactors = FALSE))
        )
        structure(nodes, type = 'connection_ggraph')
    }
}
#' @rdname get_con
#' @usage NULL
#' @export
gCon <- function(...) {
    .Deprecated('get_con')
    get_con(...)
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
getConnections <- function(layout, from, to, ...) {
    UseMethod('getConnections', layout)
}
getConnections.default <- function(layout, ...) {
    stop('Don\'t know how to get connections from an object of class ', class(layout))
}
