#' @export
gCon <- function(from = integer(), to = integer(), ...) {
    if (length(from) != length(to)) {
        stop('from and to must be of equal length')
    }
    function(layout) {
        if (length(from) == 0) return(NULL)
        connections <- getConnections(layout, from, to)
        nodes <- as.data.frame(layout)[unlist(connections), ]
        nodes$con.id <- rep(seq_along(connections), lengths(connections))
        extra <- list(...)
        nodes <- if (length(extra) > 0) {
            cbind(nodes, as.data.frame(extra))
        } else {
            nodes
        }
        structure(nodes, type = 'connection_ggraph')
    }
}
#' @export
getConnections <- function(layout, from, to, ...) {
    UseMethod('getConnections', layout)
}
#' @export
getConnections.default <- function(layout, ...) {
    stop('Don\'t know how to get connections from an object of class ', class(layout))
}
