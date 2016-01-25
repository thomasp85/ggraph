#' @rdname createLayout
#'
#' @export
#'
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
#' @importFrom igraph as_data_frame V
#'
getEdges.layout_igraph <- function(layout) {
    gr <- attr(layout, 'graph')
    edges <- igraph::as_data_frame(gr, 'edges')
    if (is.character(edges$from)) {
        edges$from <- match(edges$from, V(gr)$name)
        edges$to <- match(edges$to, V(gr)$name)
    }
    edges$circular <- attr(layout, 'circular')
    edges
}
#' Use igraph layout algorithms for layout_igraph
#'
#' This layout function makes it easy to apply one of the layout algorithms
#' supplied in igraph when plotting with ggraph. Layout names are auto completed
#' so there is no need to write \code{layout_with_graphopt} or
#' \code{layout_as_tree}, just \code{graphopt} and \code{tree} (though the
#' former will also work if you want to be super explicit). Circular layout is
#' only supported for tree-like layout (\code{tree} and \code{sugiyama}) and
#' will throw an error when applied to other layouts.
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'igraph'} in \code{\link{createLayout}}
#'
#' @param graph An igraph object.
#'
#' @param type The type of layout algorithm to apply. See
#' \code{\link[igraph]{layout_}} for links to the layouts supplied by igraph.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}. Only applicable to
#' \code{type = 'tree'} and \code{type = 'sugiyama'}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @param use.dummy Logical. In the case of \code{type = 'sugiyama'} should the
#' dummy-infused graph be used rather than the original. Defaults to
#' \code{FALSE}.
#'
#' @param ... Arguments passed on to the respective layout functions
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} as
#' well as any information stored as vertex attributes on the igraph object.
#'
#' @importFrom igraph layout_as_bipartite layout_as_star layout_as_tree layout_in_circle layout_nicely layout_with_dh layout_with_drl layout_with_gem layout_with_graphopt layout_on_grid layout_with_mds layout_with_sugiyama layout_on_sphere layout_randomly layout_with_fr layout_with_kk layout_with_lgl
#' @importFrom igraph vertex_attr
#'
layout_igraph_igraph <- function(graph, type, circular, offset = pi/2,
                                 use.dummy = FALSE, ...) {
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
#' Apply a dendrogram layout to layout_igraph
#'
#' This layout mimicks the \code{\link[igraph]{layout_as_tree}} algorithm
#' supplied by igraph, but puts all leaves at 0 and builds it up from there,
#' instead of starting from the root and building it from there. The height of
#' branch points are related to the maximum distance to an edge from the branch
#' node.
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'dendrogram'} in \code{\link{createLayout}}
#'
#' @param graph An igraph object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @param direction The direction to the leaves. Defaults to 'out'
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} and
#' \code{leaf} as well as any information stored as vertex attributes on the
#' igraph object.
#'
#' @importFrom igraph gorder degree neighbors
#'
layout_igraph_dendrogram <- function(graph, circular = FALSE, offset = pi/2, direction = 'out') {
    reverseDir <- if (direction == 'out') 'in' else 'out'
    nodes <- data.frame(
        x = rep(NA_real_, gorder(graph)),
        y = rep(NA_real_, gorder(graph)),
        leaf = degree(graph, mode = direction) == 0,
        stringsAsFactors = FALSE
    )
    startnode <- which(degree(graph, mode = reverseDir) == 0)
    if (length(startnode)  < 1) stop('No root nodes in graph')
    recurse_layout <- function(gr, node, layout, direction) {
        children <- as.numeric(neighbors(gr, node, direction))
        if (length(children) == 0) {
            x <- if (all(is.na(layout$x[layout$leaf]))) {
                1
            } else {
                max(layout$x[layout$leaf], na.rm = TRUE) + 1
            }
            layout$x[node] <- x
            layout$y[node] <- 0
            layout
        } else {
            childrenMissing <- children[is.na(layout$x[children])]
            for (i in childrenMissing) {
                layout <- recurse_layout(gr, i, layout, direction)
            }
            layout$x[node] <- mean(layout$x[children])
            layout$y[node] <- max(layout$y[children]) + 1
            layout
        }
    }
    for (i in startnode) {
        nodes <- recurse_layout(graph, i, nodes, direction = direction)
    }
    if (circular) {
        radial <- radial_trans(r.range = rev(range(nodes$y)),
                               a.range = range(nodes$x),
                               offset = offset)
        coords <- radial$transform(nodes$y, nodes$x)
        nodes$x <- coords$x
        nodes$y <- coords$y
    }
    extraData <- as.data.frame(vertex_attr(graph))
    if (nrow(extraData) == 0) extraData <- data.frame(row.names = seq_len(nrow(nodes)))
    nodes <- cbind(nodes, extraData)
    nodes$circular <- circular
    nodes
}
#' Manually specify a layout for layout_igraph
#'
#' This layout function lets you pass the node positions in manually. Each row
#' in the supplied data frame will correspond to a vertex in the igraph object
#' matched by index.
#'
#' @param graph An igraph object
#'
#' @param node.positions A data.frame with the columns \code{x} and \code{y}
#' (additional columns are ignored).
#'
#' @param circular Ignored
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} as
#' well as any information stored as vertex attributes on the igraph object.
#'
#' @importFrom igraph gorder vertex_attr
#'
layout_igraph_manual <- function(graph, node.positions, circular) {
    if (circular) {
        warning('circular argument ignored for manual layout')
    }
    if (!inherits(node.positions, 'data.frame')) {
        stop('node.positions must be supplied as data.frame')
    }
    if (gorder(graph) != nrow(node.positions)) {
        stop('Number of rows in node.position must correspond to number of nodes in graph')
    }
    if (!all(c('x', 'y') %in% names(node.positions))) {
        stop('node.position must contain the columns "x" and "y"')
    }
    layout <- data.frame(x = node.positions$x, y = node.positions$y)
    extraData <- as.data.frame(vertex_attr(graph))
    if (nrow(extraData) == 0) extraData <- data.frame(row.names = seq_len(nrow(layout)))
    layout <- cbind(layout, extraData)
    layout$circular <- FALSE
    layout
}
#' Place nodes on a line or circle
#'
#' This layout puts all nodes on a line, possibly sorted by a node attribute. If
#' \code{circular = TRUE} the nodes will be laid out on the unit circle instead.
#' In the case where the \code{sort.by} attribute is numeric, the numeric values
#' will be used as the x-position and it is thus possible to have uneven spacing
#' between the nodes.
#'
#' @param graph An igraph object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}.
#'
#' @param sort.by The name of a vertex attribute to sort the nodes by.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} as
#' well as any information stored as vertex attributes on the igraph object.
#'
#' @importFrom igraph vertex_attr_names vertex_attr gorder
#'
layout_igraph_linear <- function(graph, circular, sort.by = NULL, offset = pi/2) {
    if (!is.null(sort.by)) {
        if (!sort.by %in% vertex_attr_names(graph)) {
            stop('sort.by must be a vertex attribute of the graph')
        }
        sort.by <- vertex_attr(graph, sort.by)
        if (is.numeric(sort.by)) {
            x <- sort.by
        } else {
            x <- order(sort.by)
        }
    } else {
        x <- seq_len(gorder(graph))
    }
    nodes <- data.frame(x = x, y = 0)
    if (circular) {
        radial <- radial_trans(r.range = rev(range(nodes$y)),
                               a.range = range(nodes$x),
                               offset = offset)
        coords <- radial$transform(nodes$y, nodes$x)
        nodes$x <- coords$x
        nodes$y <- coords$y
    }
    extraData <- as.data.frame(vertex_attr(graph))
    if (nrow(extraData) == 0) extraData <- data.frame(row.names = seq_len(nrow(nodes)))
    nodes <- cbind(nodes, extraData)
    nodes$circular <- circular
    nodes
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
