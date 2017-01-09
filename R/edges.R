#' Create edge extractor function
#'
#' This function returns another function that can extract edges from a
#' ggraph_layout object. The functionality of the returned function is decided
#' by the arguments to \code{gEdges}. The need for \code{gEdges} is mainly to
#' pass to the \code{data} argument of the different \code{geom_edge_*}
#' functions in order to present them with the right kind of data. In general
#' each \code{geom_edge_*} has the default set correctly so there is only need
#' to modify the data argument if edges should be filtered by start and end node
#' parameters, if node parameters should be added to the edge data or if
#' parallel edges should be collapsed.
#'
#' @details
#' There are two types of return formats possible for the result of the returned
#' function:
#'
#' \describe{
#'   \item{short}{In this format each edge is described in one line in the
#'   format expected for \code{\link[ggplot2]{geom_segment}}, that is, the start
#'   node position is encoded in the \code{x} and \code{y} column and the end
#'   node position is encoded in the \code{xend} and \code{yend} column. If node
#'   parameters are added to the edge the name of the parameters will be
#'   prefixed with \code{node1.} for the start node and \code{node2.} for the
#'   end node.}
#'   \item{long}{In this format each edge consists of two rows with matching
#'   \code{edge.id} value. The start and end position are both encoded in the
#'   \code{x} and \code{y} column. The relative position of the rows determines
#'   which is the start and end node, the first occuring being the start node.
#'   If node parameters are added to the edge data the name of the parameters
#'   will be prefixed with \code{node.}.}
#' }
#'
#' Edges can be filtered upfront based on the parameters of the start and end
#' nodes without the need to add these parameters to the edge data. This can be
#' done by passing a boolean expression to the nodeFilter argument. The
#' expression will be evaluated in an environment where the parameters of the
#' start node is stored in node1 and the parameters of the end node is stored in
#' node2. In order to e.g. only return edges that joins nodes of the same class
#' you can use \code{node1$class == node2$class}, assuming that node class is
#' stored in a class parameter.
#'
#' Node parameters can be added to the edge data in order to either format edge
#' aesthetics according to start or end node parameters, or interpolate edge
#' aesthetics between start and end node parameters. Including node parameters
#' is done by supplying a character vector with the name(s) of the node
#' parameters to add to the edge data. The parameters will be prefixed to avoid
#' name clash with edge parameters. The prefix depends on the format (see
#' above).
#'
#' If the graph is not simple (it contains at most one edge between each node
#' pair) it can be collapsed so either all edges between two nodes or all edges
#' of the same direction between two nodes are merged. The edge parameters are
#' taken from the first occuring edge, so if some more sophisticated summary is
#' needed it is suggested that the graph be tidied up before plotting with
#' ggraph.
#'
#' @param format Either \code{'short'} (the default) or \code{'long'}. See
#' details for a descriptions of the differences
#'
#' @param nodeFilter A logical expression evaluated in an environment with
#' access to the start and end node parameters. See details for more
#' information.
#'
#' @param nodePar A character vector of node parameter names to add to the edge
#' data.
#'
#' @param collapse Either \code{'none'} (the default), \code{'all'} or
#' \code{'direction'}. Specifies whether parallel edges should be merged. See
#' details for more information
#'
#' @return A data.frame with columns dependent on format, the value of
#' \code{nodePar} as well as the graph type. In addition to the columns
#' discussed in the details section, the data.frame will always contain the
#' columns \code{from}, \code{to} and \code{circular}, the two former giving the
#' indexes of the start and end node and the latter if the layout is circular
#' (needed for correct formatting of some \code{geom_edge_*}). The graph
#' dependent information is:
#'
#' \describe{
#'   \item{dendrogram}{A \code{label} column will hold the value of the
#'   \code{edgetext} attribute. In addition any value stored in the
#'   \code{edgePar} attribute will be added. Lastly a \code{direction} column
#'   will hold the relative position between the start and end nodes (needed for
#'   correct formatting of \code{\link{geom_edge_elbow}}).}
#'   \item{igraph}{All edge attributes of the original graph object is added as
#'   columns to the data.frame}
#' }
#'
#' @family extractors
#'
#' @export
#'
gEdges <- function(format = 'short', nodeFilter = NULL, nodePar = NULL, collapse = 'none') {
    if (!collapse %in% c('none', 'all', 'direction')) {
        stop('Collapse must be either "none", "all" or "direction"')
    }
    function(layout) {
        edges <- getEdges(layout)
        edges <- switch(
            collapse,
            none = edges,
            all = collapseAllEdges(edges),
            direction = collapseDirEdges(edges)
        )
        if (!is.null(nodeFilter)) {
            keep <- with(
                list(node1 = layout[edges$from, ], node2 = layout[edges$to, ]),
                nodeFilter
            )
            edges <- edges[keep, ]
        }
        edges <- switch(
            format,
            short = formatShortEdges(edges, layout, nodePar),
            long = formatLongEdges(edges, layout, nodePar),
            stop('Unknown format. Use either "short" or "long"')
        )
        structure(edges, type_ggraph = 'edge_ggraph')
    }
}
getEdges <- function(layout) {
    UseMethod('getEdges', layout)
}
getEdges.default <- function(layout) {
    attr(layout, 'edges')
}
checkShortEdges <- function(edges) {
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
checkLongEdges <- function(edges) {
    if (!inherits(edges, 'data.frame')) {
        stop('edges must by of class data.frame', call. = FALSE)
    }
    if (!all(c('edge.id', 'node', 'x', 'y', 'circular') %in% names(edges))) {
        stop('edges must contain the columns edge.id, node, x, y and circular', call. = FALSE)
    }
    if (all(range(table(edges$edge.id)) == 2)) {
        stop('Each edge must consist of two rows')
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
formatShortEdges <- function(edges, layout, nodePar) {
    edges <- addEdgeCoordinates(edges, layout)
    if (!is.null(nodePar)) {
        nodes1 <- layout[edges$from, nodePar, drop = FALSE]
        names(nodes1) <- paste0('node1.', names(nodes1))
        nodes2 <- layout[edges$to, nodePar, drop = FALSE]
        names(nodes2) <- paste0('node2.', names(nodes2))
        edges <- cbind(edges, nodes1, nodes2)
    }
    rownames(edges) <- NULL
    checkShortEdges(edges)
}
formatLongEdges <- function(edges, layout, nodePar) {
    from <- cbind(edge.id = seq_len(nrow(edges)),
                  node = edges$from,
                  layout[edges$from, c('x', 'y')],
                  edges)
    to <- cbind(edge.id = seq_len(nrow(edges)),
                node = edges$to,
                layout[edges$to, c('x', 'y')],
                edges)
    edges <- rbind(from, to)
    if (!is.null(nodePar)) {
        node <- layout[edges$node, nodePar, drop = FALSE]
        names(node) <- paste0('node.', names(node))
        edges <- cbind(edges, node)
    }
    rownames(edges) <- NULL
    edges[order(edges$edge.id), ]
}
completeEdgeAes <- function(aesthetics) {
    if (is.null(aesthetics)) return(aesthetics)
    if (any(names(aesthetics) == 'color')) {
        names(aesthetics)[names(aesthetics) == 'color'] <- 'colour'
    }
    shortNames <- names(aesthetics) %in% c(
        'colour', 'fill', 'linetype', 'shape', 'size', 'width', 'alpha'
    )
    names(aesthetics)[shortNames] <- paste0('edge_', names(aesthetics)[shortNames])
    aesthetics
}
#' @importFrom dplyr %>% group_by_ top_n ungroup
collapseAllEdges <- function(edges) {
    from <- pmin(edges$from, edges$to)
    to <- pmax(edges$to, edges$from)
    id <- paste(from ,to, sep='-')
    if (anyDuplicated(id)) {
        edges$.id <- id
        edges <- edges %>% group_by_(~.id) %>%
            top_n(1) %>%
            ungroup()
    }
    as.data.frame(edges)
}
#' @importFrom dplyr %>% group_by_ top_n ungroup
collapseDirEdges <- function(edges) {
    id <- paste(edges$from ,edges$to, sep='-')
    if (anyDuplicated(id)) {
        edges$.id <- id
        edges <- edges %>% group_by_(~.id) %>%
            top_n(1) %>%
            ungroup()
    }
    as.data.frame(edges)
}
