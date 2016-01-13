#' @export
createLayout.igraph <- function(graph, layout, circular = FALSE, ...) {
    if (inherits(layout, 'function')) {
        layout <- layout(graph, circular = circular, ...)
    } else if (inherits(layout, 'character')) {
        if (is.igraphlayout(layout)) {
            layout <- layout_igraph_igraph(graph, layout, circular, ...)
        } else {
            layoutName <- paste0('layout_igraph_', layout)
            layout <- do.call(layoutName, list(graph, circular = circular, ...))
        }
    } else {
        stop('Unknown layout')
    }
    attr(layout, 'graph') <- graph
    attr(layout, 'circular') <- circular
    class(layout) <- c(
        'layout_igraph',
        'layout_ggraph',
        'data.frame'
    )
    checkLayout(layout)
}
#' @importFrom igraph as_data_frame
#' @export
getEdges.layout_igraph <- function(layout) {
    edges <- as_data_frame(attr(layout, 'graph'), 'edges')
    edges$circular <- attr(layout, 'circular')
    checkEdges(addEdgeCoordinates(edges, layout))
}
#' @importFrom igraph layout_as_bipartite layout_as_star layout_as_tree layout_in_circle layout_nicely layout_with_dh layout_with_drl layout_with_gem layout_with_graphopt layout_on_grid layout_with_mds layout_with_sugiyama layout_on_sphere layout_randomly layout_with_fr layout_with_kk layout_with_lgl
#' @importFrom igraph vertex_attr
layout_igraph_igraph <- function(graph, type, circular, use.dummy = FALSE, ...) {
    type <- as.igraphlayout(type)
    layout <- do.call(type, list(graph, ...))
    if (type == 'layout_with_sugiyama') {
        if (use.dummy) {
            layout <- layout$layout.dummy
            graph <- layout$graph
        } else {
            layout <- layout$layout
        }
    }
    extraData <- as.data.frame(vertex_attr(graph))
    if (nrow(extraData) == 0) extraData <- data.frame(row.names = seq_len(nrow(layout)))
    layout <- cbind(x=layout[,1], y=layout[,2], extraData)
    if (circular) {
        if (!type %in% c('layout_as_tree', 'layout_with_sugiyama')) {
            stop('Circular layout only applicable to tree and DAG layout')
        }
        radial <- radial_trans(r.range = rev(range(layout$y)),
                               a.range = range(layout$x),
                               offset = offset)
        coords <- radial$transform(layout$y, layout$x)
        layout$x <- coords$x
        layout$y <- coords$y
    }
    layout$circular <- circular
    layout
}
is.igraphlayout <- function(type) {
    if (type %in% igraphlayouts) {
        TRUE
    } else if (any(paste0(c('as_', 'in_', 'with_', 'on_'), type) %in% igraphlayouts)) {
        TRUE
    } else {
        FALSE
    }
}
as.igraphlayout <- function(type) {
    if (type %in% igraphlayouts) {
        layout <- type
    } else {
        newType <- paste0(c('as_', 'in_', 'with_', 'on_'), type)
        typeInd <- which(newType %in% igraphlayouts)
        if (length(typeInd) == 0) {
            stop('Cannot find igraph layout')
        }
        layout <- newType[typeInd]
    }
    paste0('layout_', layout)
}
igraphlayouts <- c(
    'as_bipartite',
    'as_star',
    'as_tree',
    'in_circle',
    'nicely',
    'with_dh',
    'with_drl',
    'with_gem',
    'with_graphopt',
    'on_grid',
    'with_mds',
    'with_sugiyama',
    'on_sphere',
    'randomly',
    'with_fr',
    'with_kk',
    'with_lgl'
)
