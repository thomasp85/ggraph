#' @rdname ggraph
#'
#' @aliases layout_dendrogram
#'
#' @export
#'
create_layout.dendrogram <- function(graph, layout, circular = FALSE, ...) {
    graph <- identifyNodes(graph)
    if (inherits(layout, 'function')) {
        layout <- layout(graph, circular = circular, ...)
    } else if (inherits(layout, 'character')) {
        layoutName <- paste0('layout_dendrogram_', layout)
        layout <- do.call(layoutName, list(graph, circular = circular, ...))
    } else {
        stop('Unknown layout')
    }
    layout$ggraph.index <- seq_len(nrow(layout))
    attr(layout, 'graph') <- graph
    attr(layout, 'circular') <- circular
    class(layout) <- c(
        'layout_dendrogram',
        'layout_ggraph',
        'data.frame'
    )
    checkLayout(layout)
}
getEdges.layout_dendrogram <- function(layout) {
    edges <- getLinks(attr(layout, 'graph'))
    extraPar <- bind_rows(lapply(edges$edgePar, as.data.frame, stringsAsFactors = FALSE))
    edges$edgePar <- NULL
    edges <- cbind(edges, extraPar)
    edges$circular <- attr(layout, 'circular')
    edges
}
#' @rdname layout_dendrogram_dendrogram
#'
#' @param ... Parameters passed on to layout_dendrogram_dendrogram
layout_dendrogram_auto <- function(graph, circular, ...) {
    message('Using `dendrogram` as default layout')
    layout_dendrogram_dendrogram(graph, circular = circular, ...)
}
#' Dendrogram layout for layout_dendrogram
#'
#' This layout positions the branches and leafs according to the values given in
#' the \code{height} attribute of the dendrogram object. If \code{repel = FALSE}
#' the layout is equivalent to the one shown with the plot function on
#' dendrogram objects.
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'dendrogram'} in \code{\link{create_layout}}
#'
#' @param graph A dendrogram object.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. Defaults to \code{FALSE}.
#'
#' @param offset If \code{circular = TRUE}, where should it begin. Defaults to
#' \code{pi/2} which is equivalent to 12 o'clock.
#'
#' @param repel Logical. Should leafs be distanced from their neighbors as a
#' function of their distance in the tree. Defaults to \code{FALSE}.
#'
#' @param ratio If \code{repel = TRUE}, the strength of the repelation. Defaults
#' to 1.
#'
#' @return A data.frame with the columns \code{x}, \code{y}, \code{circular},
#' \code{label}, \code{members}, \code{leaf} as well as any value stored in the
#' nodePar attribute of the dendrogram.
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggforce radial_trans
#'
layout_dendrogram_dendrogram <- function(graph, circular = FALSE, offset = pi/2, repel = FALSE, ratio = 1) {
    if (repel) {
        heights <- getHeights(graph)
        pad <-  min(heights[heights != 0])/2
    } else {
        pad <- 0
    }
    graph <- setCoord(graph, repel = repel, pad = pad, ratio = ratio)
    layout <- getCoords(graph)
    extraPar <- lapply(layout$nodePar, as.data.frame, stringsAsFactors = FALSE)
    names(extraPar) <- seq_along(extraPar)
    extraPar <- bind_rows(extraPar)
    extraPar$ggraph.dummy <- NULL
    layout$nodePar <- NULL
    layout <- cbind(layout, extraPar)
    layout <- layout[order(layout$ggraph.id), ]
    if (circular) {
        radial <- radial_trans(r.range = rev(range(layout$y)),
                               a.range = range(layout$x),
                               offset = offset,
                               pad = if (repel) (attr(graph, 'height')/2)*ratio else 0.5)
        coords <- radial$transform(layout$y, layout$x)
        layout$x <- coords$x
        layout$y <- coords$y
    }
    layout$circular <- circular
    layout[, !names(layout) %in% c('ggraph.id')]
}
#' Even layout for layout_dendrogram
#'
#' This layout sets the heights of the branch points to be the maximum distance
#' to a leaf. In this way the branch points are spread out evenly in the
#' y-direction. After modifying the height it calls
#' \code{\link{layout_dendrogram_dendrogram}}.
#'
#' @note This function is not intended to be used directly but by setting
#' \code{layout = 'even'} in \code{\link{create_layout}}
#'
#' @param graph A dendrogram object
#'
#' @param ... parameters passed on to \code{\link{layout_dendrogram_dendrogram}}
#'
#' @return A data.frame as \code{\link{layout_dendrogram_dendrogram}}
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggforce radial_trans
#'
layout_dendrogram_even <- function(graph, ...) {
    graph <- spreadHeights(graph)
    layout_dendrogram_dendrogram(graph, ...)
}
#' @importFrom stats is.leaf
identifyNodes <- function(den, start = 1) {
    if (is.leaf(den)) {
        attr(den, 'ggraph.id') <- start
    } else {
        den[[1]] <- identifyNodes(den[[1]], start)
        den[[2]] <- identifyNodes(den[[2]], attr(den[[1]], 'ggraph.id') + 1)
        attr(den, 'ggraph.id') <- attr(den[[2]], 'ggraph.id') + 1
    }
    den
}
#' @importFrom stats is.leaf
setCoord <- function(den, offset = 0, repel = TRUE, pad = 0, ratio = 1) {
    if (is.leaf(den)) {
        attr(den, 'ggraph.coord') <- offset
        attr(den, 'rightmost') <- offset
    } else {
        den[[1]] <- setCoord(den[[1]], offset, repel = repel, pad = pad, ratio = ratio)
        offset <- attr(den[[1]], 'rightmost')
        offset <- if (repel) {
            offset + (attr(den, 'height') + pad) * ratio
        } else {
            offset + 1 + pad
        }
        den[[2]] <- setCoord(den[[2]], offset, repel = repel, pad = pad, ratio = ratio)
        attr(den, 'ggraph.coord') <- mean(unlist(lapply(den, attr, which = 'ggraph.coord')))
        attr(den, 'rightmost') <- attr(den[[2]], 'rightmost')
    }
    den
}
#' @importFrom stats is.leaf
getCoords <- function(den) {
    id <- attr(den, 'ggraph.id')
    label <- attr(den, 'label')
    if (is.null(label)) label <- ''
    members <- attr(den, 'members')
    nodePar <- attr(den, 'nodePar')
    if (is.null(nodePar)) nodePar <- data.frame(ggraph.dummy = 1)
    if (is.leaf(den)) {
        list(
            x = attr(den, 'ggraph.coord'),
            y = attr(den, 'height'),
            ggraph.id = id,
            leaf = TRUE,
            label = label,
            members = members,
            nodePar = list(nodePar)
        )
    } else {
        coord1 <- getCoords(den[[1]])
        coord2 <- getCoords(den[[2]])
        list(
            x = c(coord1$x, coord2$x, attr(den, 'ggraph.coord')),
            y = c(coord1$y, coord2$y, attr(den, 'height')),
            ggraph.id = c(coord1$ggraph.id, coord2$ggraph.id, id),
            leaf = c(coord1$leaf, coord2$leaf, FALSE),
            label = c(coord1$label, coord2$label, label),
            members = c(coord1$members, coord2$members, members),
            nodePar = c(coord1$nodePar, coord2$nodePar, list(nodePar))
        )
    }
}
#' @importFrom stats is.leaf
getLinks <- function(den) {
    id <- attr(den, 'ggraph.id')
    if (is.leaf(den)) {
        data.frame(row.names = 1)
    } else {
        conn1 <- getLinks(den[[1]])
        conn2 <- getLinks(den[[2]])
        list(
            from = c(conn1$from, conn2$from, rep(id, 2)),
            to = c(conn1$to, conn2$to, unlist(lapply(den, attr, which = 'ggraph.id'))),
            label = c(conn1$label, conn2$label, unlist(lapply(den, function(subden) {
                lab <- attr(subden, 'edgetext')
                if (is.null(lab)) '' else lab
            }))),
            direction = c(conn1$direction, conn2$direction, c('right', 'left')),
            edgePar = c(conn1$edgePar, conn2$edgePar, lapply(den, function(subden) {
                par <- attr(subden, 'edgePar')
                if (is.null(par)) data.frame(row.names = 1) else par
            }))
        )
    }
}
#' @importFrom stats is.leaf
spreadHeights <- function(den) {
    if (is.leaf(den)) {
        attr(den, 'height') <- 0
    } else {
        den[[1]] <- spreadHeights(den[[1]])
        den[[2]] <- spreadHeights(den[[2]])
        attr(den, 'height') <- max(sapply(den, attr, 'height')) + 1
    }
    den
}
#' @importFrom stats is.leaf
getHeights <- function(den) {
    if (is.leaf(den)) {
        attr(den, 'height')
    } else {
        c(getHeights(den[[1]]), getHeights(den[[2]]), attr(den, 'height'))
    }
}
#' Convert a dendrogram into an igraph object
#'
#' This small helper function converts a dendrogram into an igraph object with
#' the same node indexes as would be had were the dendrogram used directly in
#' a ggraph plot. The nodes would have the same attributes as would have been
#' calculated had the dendrogram been used in layout creation, meaning that e.g.
#' it contains a leaf attribute which is \code{TRUE} for leaf nodes and
#' \code{FALSE} for the rest.
#'
#' @param den A dendrogram object
#'
#' @param even Logical should the position information be calculated based on an
#' even layout (see \code{\link{layout_dendrogram_even}}).
#'
#' @param ... Additional parameters to pass off to
#' \code{\link{layout_dendrogram_dendrogram}}
#'
#' @return An igraph object.
#'
#' @importFrom igraph graph_from_data_frame
#' @export
den_to_igraph <- function(den, even = FALSE, ...) {
    layout <- if (even) {
        create_layout(den, 'even', ...)
    } else {
        create_layout(den, 'dendrogram', ...)
    }
    edges <- getEdges(layout)
    names(layout)[1:2] <- paste0('layout.', names(layout)[1:2])
    graph_from_data_frame(edges, vertices = cbind(node.name = seq_len(nrow(layout)), layout))
}
