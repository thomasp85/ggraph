#' Apply a dendrogram layout to layout_tbl_graph
#'
#' This layout mimicks the \code{\link[igraph]{layout_as_tree}} algorithm
#' supplied by igraph, but puts all leaves at 0 and builds it up from there,
#' instead of starting from the root and building it from there. The height of
#' branch points are related to the maximum distance to an edge from the branch
#' node, or read from a node variable.
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'dendrogram'} in \code{\link{create_layout}}
#'
#' @param graph A tbl_graph object
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @param height The node variable holding the height of each node in the
#' dendrogram. If \code{NA} it will be calculated as the maximal distance to a
#' leaf.
#'
#' @param repel Should leafs repel each other relative to the height of their
#' common ancestor. Will emphasize clusters
#'
#' @param ratio The strength of repulsion. Higher values will give more defined
#' clusters
#'
#' @param direction The direction to the leaves. Defaults to 'out'
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular} and
#' \code{leaf} as well as any information stored as node variables on the
#' tbl_graph
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder degree neighbors
#' @importFrom rlang enquo eval_tidy
#'
layout_tbl_graph_dendrogram <- function(graph, circular = FALSE, offset = pi/2, height = NA, repel = FALSE, ratio = 1, direction = 'out') {
    height <- enquo(height)
    reverseDir <- if (direction == 'out') 'in' else 'out'
    nodes <- data.frame(
        x = rep(NA_real_, gorder(graph)),
        y = eval_tidy(height, .N()),
        leaf = degree(graph, mode = direction) == 0,
        stringsAsFactors = FALSE
    )
    if (all(is.na(nodes$y))) {
        nodes$y <- vapply(seq_len(gorder(graph)), function(i) {
            max(bfs(graph, i, direction, unreachable = FALSE, order = FALSE, dist = TRUE)$dist, na.rm = TRUE)
        }, numeric(1))
    }
    if (repel) {
        pad <- min(nodes$y[nodes$y != 0])/2
    } else {
        pad <- 0
    }
    startnode <- which(degree(graph, mode = reverseDir) == 0)
    if (length(startnode)  < 1) stop('No root nodes in graph')
    recurse_layout <- function(gr, node, layout, direction, offset = 0) {
        children <- as.numeric(neighbors(gr, node, direction))
        if (length(children) == 0) {
            layout$x[node] <- offset
            layout
        } else {
            childrenMissing <- children[is.na(layout$x[children])]
            for (i in childrenMissing) {
                layout <- recurse_layout(gr, i, layout, direction, offset)
                offset <- if (repel) {
                    max(layout$x[layout$leaf], na.rm = TRUE) + (layout$y[node] + pad) * ratio
                } else {
                    max(layout$x[layout$leaf], na.rm = TRUE) + 1 + pad
                }
            }
            layout$x[node] <- mean(range(layout$x[children]))
            layout
        }
    }
    offset <- 0
    for (i in startnode) {
        nodes <- recurse_layout(graph, i, nodes, direction = direction, offset)
        offset <- max(nodes$x[nodes$leaf], na.rm = TRUE) + 1
    }
    graph <- add_direction(graph, nodes)
    if (circular) {
        radial <- radial_trans(r.range = rev(range(nodes$y)),
                               a.range = range(nodes$x),
                               offset = offset)
        coords <- radial$transform(nodes$y, nodes$x)
        nodes$x <- coords$x
        nodes$y <- coords$y
    }
    extraData <- as_tibble(graph, active = 'nodes')
    nodes <- cbind(nodes, extraData)
    nodes$circular <- circular
    attr(nodes, 'graph') <- graph
    nodes
}
