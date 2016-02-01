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
#' @param algorithm The type of layout algorithm to apply. See
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
layout_igraph_igraph <- function(graph, algorithm, circular, offset = pi/2,
                                 use.dummy = FALSE, ...) {
    algorithm <- as.igraphlayout(algorithm)
    layout <- do.call(algorithm, list(graph, ...))
    if (algorithm == 'layout_with_sugiyama') {
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
        if (!algorithm %in% c('layout_as_tree', 'layout_with_sugiyama')) {
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
#' @param use.numeric Logical. Should a numeric sort.by attribute be used as the
#' actual x-coordinates in the layout. May lead to overlapping nodes. Defaults
#' to FALSE
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} as
#' well as any information stored as vertex attributes on the igraph object.
#'
#' @importFrom igraph vertex_attr_names vertex_attr gorder
#'
layout_igraph_linear <- function(graph, circular, sort.by = NULL, use.numeric = FALSE, offset = pi/2) {
    if (!is.null(sort.by)) {
        if (!sort.by %in% vertex_attr_names(graph)) {
            stop('sort.by must be a vertex attribute of the graph')
        }
        sort.by <- vertex_attr(graph, sort.by)
        if (is.numeric(sort.by) && use.numeric) {
            x <- sort.by
        } else {
            x <- order(order(sort.by))
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
#' @importFrom igraph gorder as_edgelist delete_vertex_attr
layout_igraph_treemap <- function(graph, algorithm = 'split', weight = NULL, circular = FALSE, sort.by = NULL, mode = 'out', height = 1, width = 1) {
    graph <- graph_to_tree(graph, mode)
    if (is.named(graph)) graph <- delete_vertex_attr(graph, 'name')
    hierarchy <- tree_to_hierarchy(graph, mode, sort.by, weight)
    layout <- switch(
        algorithm,
        split = splitTreemap(hierarchy$parent, hierarchy$order, hierarchy$weight, width, height),
        stop('Unknown algorithm')
    )
    layout <- data.frame(x = layout[, 1] + layout[, 3]/2,
                         y = layout[, 2] + layout[, 4]/2,
                         width = layout[, 3],
                         height = layout[, 4],
                         circular = FALSE,
                         leaf = degree(graph, mode = mode) == 0)
    extraData <- as.data.frame(vertex_attr(graph))
    if (nrow(extraData) == 0) extraData <- data.frame(row.names = seq_len(nrow(layout)))
    layout <- cbind(layout, extraData)
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
#' @importFrom igraph degree unfold_tree components induced_subgraph vertex_attr_names
graph_to_tree <- function(graph, mode) {
    if (!is.directed(graph)) {
        stop('Graph must be directed')
    }
    parentDir <- if (mode == 'out') 'in' else 'out'
    comp <- components(graph, 'weak')
    if (comp$no > 1) {
        message('Multiple components in graph. Choosing the first')
        graph <- induced_subgraph(graph, which(comp$membership == 1))
    }
    nParents <- degree(graph, mode = parentDir)
    if (!any(nParents == 0)) {
        stop('No root in graph. Provide graph with one parentless node')
    }
    if (any(nParents > 1)) {
        message('Multiple parents. Unfolding graph')
        root <- which(degree(graph, mode = parentDir) == 0)
        if (length(root) > 1) {
            message('Multiple roots in graph. Choosing the first')
            root <- root[1]
        }
        tree <- unfold_tree(graph, mode = mode, roots = root)
        vAttr <- lapply(vertex_attr(graph), `[`, i = tree$vertex_index)
        vertex_attr(tree$tree) <- vAttr
        graph <- tree$tree
    }
    graph
}
tree_to_hierarchy <- function(graph, mode, sort.by, weight) {
    parentCol <- if (mode == 'out') 1 else 2
    nodeCol <- if (mode == 'out') 2 else 1
    edges <- as_edgelist(graph)
    hierarchy <- data.frame(parent = rep(-1, gorder(graph)))
    hierarchy$parent[edges[, nodeCol]] <- edges[, parentCol] - 1
    if (is.null(sort.by)) {
        hierarchy$order <- seq_len(nrow(hierarchy))
    } else {
        hierarchy$order <- order(vertex_attr(graph, sort.by))
    }
    leaf <- degree(graph, mode = mode) == 0
    if (is.null(weight)) {
        hierarchy$weight <- 0
        hierarchy$weight[leaf] <- 1
    } else {
        weight <- vertex_attr(graph, weight)
        if (!is.numeric(weight)) {
            stop('Weight must be numeric')
        }
        hierarchy$weight <- weight
        if (any(hierarchy$weight[!leaf] != 0)) {
            message('Non-leaf weights ignored')
        }
        hierarchy$weight[!leaf] <- 0
    }
    hierarchy
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
