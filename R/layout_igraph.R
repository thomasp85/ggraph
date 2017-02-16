#' @rdname ggraph
#' @aliases layout_igraph
#'
#' @importFrom igraph V<-
#' @export
#'
create_layout.igraph <- function(graph, layout, circular = FALSE, ...) {
    V(graph)$ggraph.orig_index <- seq_len(gorder(graph))
    if (inherits(layout, 'function')) {
        layout <- layout(graph, circular = circular, ...)
    } else if (inherits(layout, 'character')) {
        if (is.igraphlayout(layout)) {
            layout <- layout_igraph_igraph(graph, layout, circular, ...)
        } else {
            graph <- prepare_graph(graph, layout, ...)
            layoutName <- paste0('layout_igraph_', layout)
            layout <- do.call(layoutName, list(graph, circular = circular, ...))
        }
    } else {
        stop('Unknown layout')
    }
    layout$ggraph.index <- seq_len(nrow(layout))
    if (is.null(attr(layout, 'graph'))) {
        attr(layout, 'graph') <- graph
    }
    attr(layout, 'circular') <- circular
    class(layout) <- c(
        'layout_igraph',
        'layout_ggraph',
        'data.frame'
    )
    checkLayout(layout)
}
#' @importFrom igraph as_edgelist V
#'
getEdges.layout_igraph <- function(layout) {
    gr <- attr(layout, 'graph')
    edges <- as.data.frame(as_edgelist(gr, names = FALSE))
    names(edges) <- c('from', 'to')
    eattr <- attr_df(gr, 'edge')
    edges <- cbind(edges, eattr)
    edges$circular <- attr(layout, 'circular')
    edges
}
#' @importFrom igraph shortest_paths
getConnections.layout_igraph <- function(layout, from, to, weight = NULL, mode = 'all') {
    from <- match(from, layout$ggraph.orig_index)
    to <- match(to, layout$ggraph.orig_index)
    if (is.null(weight)) {
        weight <- NA
    } else {
        weight <- layout[[weight]]
    }
    graph <- attr(layout, 'graph')
    to <- split(to, from)
    connections <- lapply(seq_along(to), function(i) {
        paths <- shortest_paths(graph, as.integer(names(to)[i]), to[[i]], mode = mode, weights = weight)$vpath
        lapply(paths, as.numeric)
    })
    unlist(connections, recursive = FALSE)
}
#' @rdname layout_igraph_igraph
layout_igraph_auto <- function(graph, circular, ...) {
    message('Using `nicely` as default layout')
    layout_igraph_igraph(graph, algorithm = 'nicely', circular = circular, ...)
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
#' @details
#' igraph provides a huge amount of possible layouts. They are all briefly
#' described below:
#'
#' \strong{Hierarchical layouts}
#'
#' \describe{
#'   \item{\code{tree}}{Uses the \emph{Reingold-Tilford} algorithm to place the
#'   nodes below their parent with the parent centered above its children. See
#'   \code{\link[igraph]{as_tree}}}
#'   \item{\code{sugiyama}}{Designed for directed acyclic graphs (that is,
#'   hierarchies where multiple parents are allowed) it minimizes the number of
#'   crossing edges. See \code{\link[igraph]{with_sugiyama}}}
#' }
#'
#' \strong{Standard layouts}
#'
#' \describe{
#'   \item{\code{bipartite}}{Minimize edge-crossings in a simple two-row (or
#'   column) layout for bipartite graphs. See \code{\link[igraph]{as_bipartite}}}
#'   \item{\code{star}}{Place one node in the center and the rest equidistantly
#'   around it. See \code{\link[igraph]{as_star}}}
#'   \item{\code{circle}}{Place nodes in a circle in the order of their index.
#'   Consider using \code{\link{layout_igraph_linear}} with \code{circular=TRUE}
#'   for more control. See \code{\link[igraph]{in_circle}}}
#'   \item{\code{nicely}}{Tries to pick an appropriate layout. See
#'   \code{\link[igraph]{nicely}} for a description of the simpe decision tree
#'   it uses}
#'   \item{\code{dh}}{Uses \emph{Davidson and Harels} simulated annealing
#'   algorithm to place nodes. See \code{\link[igraph]{with_dh}}}
#'   \item{\code{gem}}{Place nodes on the plane using the GEM force-directed
#'   layout algorithm. See \code{\link[igraph]{with_gem}}}
#'   \item{\code{graphopt}}{Uses the Graphopt algorithm based on alternating
#'   attraction and repulsion to place nodes. See
#'   \code{\link[igraph]{with_graphopt}}}
#'   \item{\code{grid}}{Place nodes on a rectangular grid. See
#'   \code{\link[igraph]{on_grid}}}
#'   \item{\code{mds}}{Perform a multidimensional scaling of nodes using either
#'   the shortest path or a user supplied distance. See
#'   \code{\link[igraph]{with_mds}}}
#'   \item{\code{sphere}}{Place nodes uniformly on a sphere - less relevant for
#'   2D visualizations of networks. See \code{\link[igraph]{on_sphere}}}
#'   \item{\code{randomly}}{Places nodes uniformly random. See
#'   \code{\link[igraph]{randomly}}}
#'   \item{\code{fr}}{Places nodes according to the force-directed algorithm of
#'   Fruchterman and Reingold. See \code{\link[igraph]{with_fr}}}
#'   \item{\code{kk}}{Uses the spring-based algorithm by Kamada and Kawai to
#'   place nodes. See \code{\link[igraph]{with_kk}}}
#'   \item{\code{drl}}{Uses the force directed algorithm from the DrL toolbox to
#'   place nodes. See \code{\link[igraph]{with_drl}}}
#'   \item{\code{lgl}}{Uses the algorithm from Large Graph Layout to place
#'   nodes. See \code{\link[igraph]{with_lgl}}}
#' }
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'igraph'} in \code{\link{create_layout}}
#'
#' @param graph An igraph object.
#'
#' @param algorithm The type of layout algorithm to apply. See
#' \code{\link[igraph]{layout_}} for links to the layouts supplied by igraph.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}. Only applicable to
#' \code{algorithm = 'tree'} and \code{algorithm = 'sugiyama'}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @param use.dummy Logical. In the case of \code{algorithm = 'sugiyama'} should the
#' dummy-infused graph be used rather than the original. Defaults to
#' \code{FALSE}.
#'
#' @param ... Arguments passed on to the respective layout functions
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} as
#' well as any information stored as vertex attributes on the igraph object.
#'
#' @family layout_igraph_*
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
    extraData <- attr_df(graph)
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
#' \code{layout = 'dendrogram'} in \code{\link{create_layout}}
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
#' @family layout_igraph_*
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
    extraData <- attr_df(graph)
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
#' @family layout_igraph_*
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
    extraData <- attr_df(graph)
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
#' @family layout_igraph_*
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
    extraData <- attr_df(graph)
    nodes <- cbind(nodes, extraData)
    nodes$circular <- circular
    nodes
}
#' Calculate nodes as rectangels subdividing that of their parent
#'
#' A treemap is a space filling hierarchical layout that maps nodes to
#' rectangles. The rectangles of the children of a node is packed into the
#' rectangle of the node so that the size of a rectangle is a function of the
#' size of the children. The size of the leaf nodes can be mapped arbitrarily
#' (defaults to 1). Many different algorithms exists for dividing a rectangle
#' into smaller bits, some optimizing the aspect ratio and some focusing on the
#' ordering of the rectangles. See details for more discussions on this. The
#' treemap layout was first developed by Ben Shneiderman for visualizing disk
#' usage in the early '90 and has seen many improvements since.
#'
#' @details
#' Different approaches to dividing the rectangles in a treemap exists; all with
#' their strengths and weaknesses. Currently only the split algorithm is
#' implemented which strikes a good balance between aspect ratio and order
#' preservation, but other, more well-known, algorithms such as squarify and
#' slice-and-dice will eventually be implemented.
#'
#' \strong{Algorithms}
#'
#' \emph{Split} (default)
#'
#' The Split algorithm was developed by Bjorn Engdahl in order to address the
#' downsides of both the original slice-and-dice algorithm (poor aspect ratio)
#' and the popular squarify algorithm (no ordering of nodes). It works by
#' finding the best cut in the ordered list of children in terms of making sure
#' that the two rectangles associated with the split will have optimal aspect
#' ratio.
#'
#'
#' @note
#' Treemap is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#' @param graph An igraph object
#'
#' @param algorithm The name of the tiling algorithm to use. Defaults to 'split'
#'
#' @param weight An optional vertex attribute to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored.
#'
#' @param sort.by The name of a vertex attribute to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. \code{'out'} (default)
#' means that parents point towards their children, while \code{'in'} means that
#' children point towards their parent.
#'
#' @param height The height of the bounding rectangle
#'
#' @param width The width of the bounding rectangle
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{width},
#' \code{height}, \code{leaf}, \code{depth}, \code{circular} as well as any
#' information stored as vertex attributes on the igraph object.
#'
#' @references
#' Engdahl, B. (2005). \emph{Ordered and unordered treemap algorithms and their
#' applications on handheld devices}. Master's Degree Project.
#'
#' Johnson, B., & Ben Shneiderman. (1991). \emph{Tree maps: A Space-Filling
#' Approach to the Visualization of Hierarchical Information Structures}. IEEE
#' Visualization, 284-291. \url{http://doi.org/10.1109/VISUAL.1991.175815}
#'
#' @family layout_igraph_*
#'
layout_igraph_treemap <- function(graph, algorithm = 'split', weight = NULL, circular = FALSE, sort.by = NULL, direction = 'out', height = 1, width = 1) {
    hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight)
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
                         leaf = degree(graph, mode = direction) == 0,
                         depth = node_depth(graph, mode = direction))
    extraData <- attr_df(graph)
    layout <- cbind(layout, extraData)
    layout
}
#' Calculate nodes as circles packed within their parent circle
#'
#' The circle packing algorithm is basically a treemap using circles instead of
#' rectangles. Due to the nature of circles they cannot be packed as efficeintly
#' leading to increased amount of "empty space" as compared to a treemap. This
#' can be beneficial though, as the added empty space can aid in visually
#' showing the hierarchy.
#'
#' @details
#' The circle packing is based on the algorithm developed by Weixin Wang and
#' collaborators which tries to find the most dense packing of circles as they
#' are added, one by one. This makes the algorithm very dependent on the order
#' in which circles are added and it is possible that layouts could sometimes
#' be optimized by choosing a different ordering. The algorithm for finding the
#' enclosing circle is that randomized incremental algorithm proposed by Emo
#' Welzl. Both of the above algorithms are the same as used in the D3.js
#' implementation of circle packing and their C++ implementation in ggraph is
#' inspired by Mike Bostocks JavaScript implementation.
#'
#' @note
#' Circle packing is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An igraph object
#'
#' @param weight An optional vertex attribute to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Ignored.
#'
#' @param sort.by The name of a vertex attribute to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. \code{'out'} (default)
#' means that parents point towards their children, while \code{'in'} means that
#' children point towards their parent.
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{r}, \code{leaf},
#' \code{depth}, \code{circular} as well as any information stored as vertex
#' attributes on the igraph object.
#'
#' @references
#' Wang, W., Wang, H. H., Dai, G., & Wang, H. (2006). \emph{Visualization of
#' large hierarchical data by circle packing}. Chi, 517-520.
#'
#' Welzl, E. (1991). \emph{Smallest enclosing disks (balls and ellipsoids)}. New
#' Results and New Trends in Computer Science, 359-370.
#'
#' @family layout_igraph_*
#'
layout_igraph_circlepack <- function(graph, weight = NULL, circular = FALSE, sort.by = NULL, direction = 'out') {
    hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight)
    layout <- circlePackLayout(hierarchy$parent, hierarchy$weight)
    layout <- data.frame(x = layout[, 1],
                         y = layout[, 2],
                         r = layout[, 3],
                         circular = FALSE,
                         leaf = degree(graph, mode = direction) == 0,
                         depth = node_depth(graph, mode = direction))
    extraData <- attr_df(graph)
    layout <- cbind(layout, extraData)
    layout
}
#' Calculate nodes as areas dividing their parent
#'
#' The partition layout is a way to show hierarchical data in the same way as
#' \code{\link{layout_igraph_treemap}}. Instead of subdividing the parent area
#' the partition layout shows the division of a nodes children next to the area
#' of the node itself. As such the node positions will be very reminicent of
#' a reingold-tilford tree layout but by plotting nodes as areas it better
#' communicate the total weight of a node by summing up all its children.
#' Often partition layouts are called icicle plots or sunburst diagrams (in case
#' a radial transform is applied).
#'
#' @note
#' partition is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An igraph object
#'
#' @param weight An optional vertex attribute to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. If \code{TRUE} the resulting layout will be a sunburst
#' diagram.
#'
#' @param height An optional vertex attribute to use as height. If \code{NULL}
#' all nodes will be given a height of 1.
#'
#' @param sort.by The name of a vertex attribute to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. \code{'out'} (default)
#' means that parents point towards their children, while \code{'in'} means that
#' children point towards their parent.
#'
#' @param const.area Logical. Should 'height' be scaled for area proportionality
#' when using \code{circular = TRUE}. Defaults to \code{TRUE}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @return If \code{circular = FALSE} A data.frame with the columns \code{x},
#' \code{y}, \code{width}, \code{height}, \code{leaf},
#' \code{depth}, \code{circular} as well as any information stored as vertex
#' attributes on the igraph object.
#' If \code{circular = TRUE} A data.frame with the columns \code{x}, \code{y},
#' \code{r0}, \code{r}, \code{start}, \code{end}, \code{leaf},
#' \code{depth}, \code{circular} as well as any information stored as vertex
#' attributes on the igraph object.
#'
#' @references
#' Kruskal, J. B., Landwehr, J. M. (1983). \emph{Icicle Plots: Better Displays
#' for Hierarchical Clustering}. American Statistician Vol 37(2), 162-168.
#' \url{http://doi.org/10.2307/2685881}
#'
#' @family layout_igraph_*
#'
#' @importFrom ggforce radial_trans
#'
layout_igraph_partition <- function(graph, weight = NULL, circular = FALSE, height = NULL, sort.by = NULL, direction = 'out', offset = pi/2, const.area = TRUE) {
    hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight, height)
    layout <- partitionTree(hierarchy$parent, hierarchy$order, hierarchy$weight, hierarchy$height)
    if (circular) {
        if (const.area) {
            y0 <- sqrt(layout[, 2])
            y1 <- sqrt(layout[, 2] + layout[, 4])
            layout[, 2] <- y0
            layout[, 4] <- y1 - y0
        }
        width_range <- c(0, max(rowSums(layout[, c(1, 3)])))
        radial <- radial_trans(r.range = c(0, 1),
                               a.range = width_range,
                               offset = offset,
                               pad = 0)
        coords <- radial$transform(layout[, 2] + layout[, 4]/2,
                                   layout[, 1] + layout[, 3]/2)
        layout <- data.frame(
            x = coords$x,
            y = coords$y,
            r0 = layout[, 2],
            r = layout[, 2] + layout[, 4],
            start = 2*pi*layout[, 1]/width_range[2],
            end = 2*pi*(layout[, 1] + layout[, 3])/width_range[2],
            circular = TRUE
        )
        layout$x[1] <- 0
        layout$y[1] <- 0
    } else {
        layout <- data.frame(
            x = layout[, 1] + layout[, 3]/2,
            y = layout[, 2] + layout[, 4]/2,
            width = layout[, 3],
            height = layout[, 4],
            circular = FALSE
        )
    }
    layout$leaf = degree(graph, mode = direction) == 0
    layout$depth = node_depth(graph, mode = direction)
    extraData <- attr_df(graph)
    layout <- cbind(layout, extraData)
    layout
}
#' Place nodes in a Hive Plot layout
#'
#' Hive plots were invented by Martin Krzywinski as a perceptually uniform and
#' scalable alternative to standard node-edge layouts. In hive plots nodes are
#' positioned on axes radiating out from a center based on their own information
#' e.g. membership of a class, size of neighborhood, etc. Edges are then drawn
#' between nodes as bezier curves. As the placement of nodes is not governed by
#' convoluted algorithms but directly reflects the qualities of the nodes itself
#' the resulting plot can be easier to interpret as well as compare to other
#' graphs.
#'
#' @details
#' In order to be able to draw all edges without edges crossing axes you should
#' not assign nodes to axes based on a variable with more than three levels.
#'
#' @param graph An igraph object
#'
#' @param axis The node attribute to use for assigning nodes to axes
#'
#' @param axis.pos The relative distance to the prior axis. Default
#' (\code{NULL}) places axes equidistant.
#'
#' @param sort.by The node attribute to use for placing nodes along their axis.
#' Defaults (\code{NULL}) places nodes sequentially.
#'
#' @param divide.by An optional node attribute to subdivide each axis by.
#'
#' @param divide.order The order the axis subdivisions should appear in
#'
#' @param normalize Logical. Should axis lengths be equal or reflect the number
#' of nodes in each axis. Defaults to \code{TRUE}.
#'
#' @param center.size The size of the blank center, that is, the start position
#' of the axes.
#'
#' @param divide.size The distance between subdivided axis segments.
#'
#' @param use.numeric Logical, If the \code{sort.by} attribute is numeric,
#' should these values be used directly in positioning the nodes along the axes.
#' Defaults to \code{FALSE} which sorts the numeric values and positions them
#' equidistant from each other.
#'
#' @param offset Change the overall rotation of the hive plot by changing the
#' offset of the first axis.
#'
#' @param split.axes Should axes be split to show edges between nodes on the
#' same axis? One of:
#' \describe{
#'   \item{\code{'none'}}{Do not split axes and show in-between edges}
#'   \item{\code{'loops'}}{Only split axes that contain in-between edges}
#'   \item{\code{'all'}}{Split all axes}
#' }
#'
#' @param split.angle The angular distance between the two axes resulting from a
#' split.
#'
#' @param circular Ignored.
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{r},
#' \code{centerSize}, \code{split}, \code{axis}, \code{section}, \code{angle},
#' \code{circular} as well as any information stored as vertex attributes on the
#' igraph object.
#'
#' @references
#' Krzywinski, M., Birol, I., Jones, SJM., and Marra, MA. (2012). \emph{Hive
#' plots-rational approach to visualizing networks}. Brief Bioinform 13 (5):
#' 627-644. \url{http://doi.org/10.1093/bib/bbr069}
#'
#' \url{http://www.hiveplot.net}
#'
#' @family layout_igraph_*
#'
#' @importFrom igraph gorder vertex_attr gsize induced_subgraph add_vertices E ends add_edges delete_edges %--% edge_attr
#' @importFrom utils tail
layout_igraph_hive <- function(graph, axis, axis.pos = NULL, sort.by = NULL, divide.by = NULL, divide.order = NULL, normalize = TRUE, center.size = 0.1, divide.size = 0.05, use.numeric = FALSE, offset = pi/2, split.axes = 'none', split.angle = pi/6, circular = FALSE) {
    axes <- split(seq_len(gorder(graph)), vertex_attr(graph, axis))
    if (is.null(axis.pos)) {
        axis.pos <- rep(1, length(axes))
    } else {
        if (length(axis.pos) != length(axes)) {
            warning("Number of axes not matching axis.pos argument. Recycling as needed")
            axis.pos <- rep(axis.pos, length.out = length(axes))
        }
    }
    axis.pos <- -cumsum(axis.pos)
    axis.pos <- c(0, axis.pos[-length(axis.pos)]) / -tail(axis.pos, 1) * 2 * pi + offset
    if (use.numeric) {
        if (is.null(sort.by) || !is.numeric(vertex_attr(graph, sort.by))) {
            stop('sort.by must be a numeric vertex attribute when use.numeric = TRUE')
        }
        numeric.range <- range(vertex_attr(graph, sort.by))
    }
    if (normalize) {
        normalizeTo <- rep(1, length(axes))
    } else {
        normalizeTo <- lengths(axes) / max(lengths(axes))
    }
    node.pos <- Map(function(nodes, axisLength, axis, angle) {
        splitAxis <- switch(
            split.axes,
            all = TRUE,
            loops = gsize(induced_subgraph(graph, nodes)) > 0,
            none = FALSE,
            stop('Unknown split argument. Use "all", "loops" or "none"')
        )
        nodeDiv <- axisLength / length(nodes)
        if (is.null(divide.by)) {
            nodeSplit <- list(`1` = nodes)
        } else {
            if (use.numeric) {
                stop('Cannot divide axis while use.numeric = TRUE')
            }
            nodeSplit <- split(nodes, vertex_attr(graph, divide.by, nodes))
            if (!is.null(divide.order)) {
                if (!all(divide.order %in% names(nodeSplit))) {
                    stop('All ', divide.by, ' levels must be present in divide.order')
                }
                nodeSplit <- nodeSplit[order(match(names(nodeSplit), divide.order))]
            }
        }
        nodePos <- lapply(nodeSplit, function(nodes) {
            if (length(nodes) == 0) return(numeric())
            if (is.null(sort.by)) {
                pos <- match(seq_along(nodes), order(nodes)) - 1
                pos <- pos * nodeDiv
            } else {
                pos <- vertex_attr(graph, sort.by, nodes)
                if (use.numeric) {
                    if (!is.numeric(pos)) {
                        stop('sort.by must contain numeric data when use.numeric = TRUE')
                    }
                    if (normalize) {
                        if (diff(range(pos)) == 0) {
                            pos <- rep(0.5, length.out = length(pos))
                        } else {
                            pos <- (pos - min(pos))/diff(range(pos))
                        }
                    } else {
                        pos <- (pos - numeric.range[1])/diff(numeric.range)
                    }
                } else {
                    pos <- match(seq_along(pos), order(pos)) - 1
                    pos <- pos * nodeDiv
                }
            }
            pos
        })
        nodePos <- Reduce(function(l, r) {
            append(l, list(r + nodeDiv + divide.size + max(l[[length(l)]])))
        }, x = nodePos[-1], init = nodePos[1])
        nodePos <- unlist(nodePos) + center.size
        data.frame(
            node = nodes,
            r = nodePos[match(nodes, unlist(nodeSplit))],
            centerSize = center.size,
            split = splitAxis,
            axis = axis,
            section = rep(names(nodeSplit), lengths(nodeSplit))[match(nodes, unlist(nodeSplit))],
            angle = angle,
            circular = FALSE,
            stringsAsFactors = FALSE
        )
    }, nodes = axes, axisLength = normalizeTo, axis = names(axes), angle = axis.pos)
    for (i in seq_along(node.pos)) {
        if (node.pos[[i]]$split[1]) {
            nNewNodes <- nrow(node.pos[[i]])
            newNodeStart <- gorder(graph) + 1
            extraNodes <- node.pos[[i]]
            extraNodes$node <- seq(newNodeStart, length.out = nNewNodes)
            vattr <- lapply(vertex_attr(graph), `[`, i = node.pos[[i]]$node)
            graph  <- add_vertices(graph, nNewNodes, attr = vattr)

            loopEdges <- E(graph)[node.pos[[i]]$node %--% node.pos[[i]]$node]
            if (length(loopEdges) != 0) {
                loopEdgesEnds <- ends(graph, loopEdges, names = FALSE)
                correctOrderEnds <- node.pos[[i]]$r[match(loopEdgesEnds[,1], node.pos[[i]]$node)] <
                    node.pos[[i]]$r[match(loopEdgesEnds[,2], node.pos[[i]]$node)]
                loopEdgesEnds <- data.frame(
                    from = ifelse(correctOrderEnds, loopEdgesEnds[,1], loopEdgesEnds[,2]),
                    to = ifelse(correctOrderEnds, loopEdgesEnds[,2], loopEdgesEnds[,1])
                )
                loopEdgesEnds$to <- extraNodes$node[match(loopEdgesEnds$to, node.pos[[i]]$node)]
                loopEdgesEnds <- matrix(c(
                    ifelse(correctOrderEnds, loopEdgesEnds$from, loopEdgesEnds$to),
                    ifelse(correctOrderEnds, loopEdgesEnds$to, loopEdgesEnds$from)
                ), nrow = 2, byrow = TRUE)
                eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(loopEdges))
                graph <- add_edges(graph, as.vector(loopEdgesEnds), attr = eattr)
                graph <- delete_edges(graph, as.numeric(loopEdges))
            }

            nodeCorrection <- unlist(lapply(node.pos[-i], function(ax) {
                correct <- if (ax$angle[1] < node.pos[[i]]$angle[1]) {
                    ax$angle[1] - node.pos[[i]]$angle[1] < -pi
                } else {
                    ax$angle[1] - node.pos[[i]]$angle[1] < pi
                }
                if (correct) ax$node
            }))
            if (length(nodeCorrection) != 0) {
                correctEdges <- E(graph)[node.pos[[i]]$node %--% nodeCorrection]
                correctEdgesEnds <- ends(graph, correctEdges, names = FALSE)
                newNodeInd <- correctEdgesEnds %in% node.pos[[i]]$node
                correctEdgesEnds[newNodeInd] <- extraNodes$node[match(correctEdgesEnds[newNodeInd], node.pos[[i]]$node)]
                eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(correctEdges))
                graph <- add_edges(graph, as.vector(t(correctEdgesEnds)), attr = eattr)
                graph <- delete_edges(graph, as.numeric(correctEdges))
            }

            node.pos[[i]]$angle <- node.pos[[i]]$angle - split.angle/2
            extraNodes$angle <- extraNodes$angle + split.angle/2
            node.pos <- append(node.pos, list(extraNodes))
        }
    }
    node.pos <- lapply(node.pos, function(nodes) {
        nodes$x <- nodes$r * cos(nodes$angle)
        nodes$y <- nodes$r * sin(nodes$angle)
        nodes
    })
    node.pos <- do.call(rbind, node.pos)
    node.pos <- node.pos[order(node.pos$node), names(node.pos) != 'node']
    extraData <- attr_df(graph)
    node.pos <- cbind(node.pos, extraData)
    attr(node.pos, 'graph') <- graph
    node.pos
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
#' @importFrom igraph gorder permute
prepare_graph <- function(graph, layout, direction = 'out', ...) {
    is_hierarchy <- layout %in% c(
        'dendrogram',
        'treemap',
        'circlepack',
        'partition'
    )
    if (is_hierarchy) {
        graph <- graph_to_tree(graph, mode = direction)
        graph <- permute(graph, match(seq_len(gorder(graph)), order(node_depth(graph, direction))))
    }
    graph
}
#' @importFrom igraph degree unfold_tree components induced_subgraph vertex_attr vertex_attr<- is.directed simplify
graph_to_tree <- function(graph, mode) {
    if (!is.directed(graph)) {
        stop('Graph must be directed')
    }
    graph <- simplify(graph)
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
#' @importFrom igraph gorder as_edgelist delete_vertex_attr is.named
tree_to_hierarchy <- function(graph, mode, sort.by, weight, height = NULL) {
    if (is.named(graph)) graph <- delete_vertex_attr(graph, 'name')
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
    if (is.null(height)) {
        hierarchy$height <- 1
    } else {
        hierarchy$height <- vertex_attr(graph, height)
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
        if (any(hierarchy$weight[leaf] == 0)) {
            stop('Leafs must have a weight')
        }
        hierarchy$weight[!leaf] <- 0
    }
    hierarchy
}
#' @importFrom igraph bfs degree
node_depth <- function(graph, mode) {
    mode_rev <- switch(
        mode,
        `in` = 'out',
        out = 'in',
        stop('unknown mode')
    )
    root <- which(degree(graph, mode = mode_rev) == 0)
    if (length(root) != 1) {
        stop('Graph must have one root', call. = FALSE)
    }
    unname(bfs(graph, root = root, dist = T)$dist)
}
#' @importFrom igraph vertex_attr edge_attr gorder gsize
attr_df <- function(gr, type = 'vertex') {
    attrList <- switch(
        type,
        vertex = vertex_attr(gr),
        edge = edge_attr(gr),
        stop('type must be either "vertex" or "edge"')
    )
    if (length(attrList) == 0) {
        nrows <- switch(
            type,
            vertex = gorder(gr),
            edge = gsize(gr),
            stop('type must be either "vertex" or "edge"')
        )
        return(data.frame(matrix(nrow = nrows, ncol = 0)))
    }
    attrList <- lapply(attrList, function(attr) {
        if (class(attr) == 'list') {
            I(attr)
        } else {
            attr
        }
    })
    as.data.frame(attrList)
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
