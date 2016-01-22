#' @export
getEdges <- function(layout) {
    UseMethod('getEdges', layout)
}
#' @export
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
#' @export
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
        edges
    }
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
    id <- paste(edgees$from ,edges$to, sep='-')
    if (anyDuplicated(id)) {
        edges$.id <- id
        edges <- edges %>% group_by_(~.id) %>%
            top_n(1) %>%
            ungroup()
    }
    as.data.frame(edges)
}
