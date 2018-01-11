#' Apply a function recursively to a tree
#'
#' This function allows for easy recursive function calling of tree-like
#' structures. The recursion can happen either downwards, calling the children
#' after the parent, or upwards, calling the parent after the children. At each
#' recursion there is access to the node(s) that was/were called before and thus
#' have already been through the function.
#'
#' @details The function is called with a set of predifined parameters along
#' with user defined ones. If `direction = 'down'` The parameters supplied
#' automatically to the function are: `node`, `parent`, `depth`
#' and `tree`, while if `direction = 'up'` the parameters are:
#' `node`, `children`, `depth` and `tree`. The nature of
#' `node`, `parent` and `children` depends on the class of
#' `tree`. If `class(tree) == 'igraph'` they will be indices of the
#' relevant vertices in the graph. If `class(tree) == 'dendrogram'` they
#' will be the actual dendrogram objects.
#'
#' @param tree A tree-like object. Currently support for igraph objects
#'
#' @param FUN The function to apply to each node. The function must return a
#' modified version of the graph if `class(tree) == 'igraph'` or a modified
#' version of the node if `class(tree) == 'dendrogram'`
#'
#' @param direction The direction of the recursion. If `direction = 'down'`
#' The parent will get handled before the children, while the reverse is true if
#' `direction = 'up'`
#'
#' @param mode For `class(tree) == 'igraph'` the directionality of the
#' edges in the graph. If `mode = 'out'` then parents points towards their
#' children, while the reverse is true for `mode = 'in'`
#'
#' @param ... Additional parameters to `FUN`
#'
#' @return A modified version of `tree` (if anything is modified in
#' `FUN`)
#'
#' @keywords internal
#'
#' @export
#'
tree_apply <- function(tree, FUN, ...) {
    .Deprecated('map_bfs', 'tidygraph')
    UseMethod('tree_apply')
}
#' @rdname tree_apply
#' @usage NULL
#' @export
treeApply <- function(...) {
    .Deprecated('tree_apply')
    tree_apply(...)
}
#' @rdname tree_apply
#' @export
tree_apply.default <- function(tree, FUN, ...) {
    stop('tree_apply not implemented for ', class(tree), ' objects')
}
#' @rdname tree_apply
#' @importFrom igraph bfs
#' @export
tree_apply.igraph <- function(tree, FUN, direction = 'down', mode = 'out', ...) {
    root <- which(degree(tree, mode = if (mode == 'out') 'in' else 'out') == 0)
    traverse <- bfs(tree, root, mode, father = TRUE, dist = TRUE)
    father <- as.integer(traverse$father)
    if (direction == 'up') {
        order <- rev(as.integer(traverse$order))
        args <- list(node = 0, children = integer(), depth = 0, tree = tree, ...)
        for (i in order) {
            args$node <- i
            args$children <- which(father == i)
            args$depth <- traverse$dist[i]
            args$tree <- do.call(FUN, args)
        }
    } else {
        order <- as.integer(traverse$order)
        args <- list(node = 0, parent = integer(), depth = 0, tree = tree, ...)
        for (i in order) {
            args$node <- i
            args$parent <- father[i]
            args$depth <- traverse$dist[i]
            args$tree <- do.call(FUN, args)
        }
    }
    args$tree
}
#' @rdname tree_apply
#' @export
tree_apply.dendrogram <- function(tree, FUN, direction = 'down', ...) {
    switch(
        direction,
        down = downApplyDen(tree, FUN, ...),
        up = upApplyDen(tree, FUN, ...)
    )
}
#' @importFrom stats is.leaf
downApplyDen <- function(node, FUN, parent = NULL, depth = 0, tree = node, ...) {
    args <- list(node = node, parent = parent, depth = depth, tree = tree, ...)
    node <- do.call(FUN, args)
    if (!is.leaf(node)) {
        for (i in seq_along(node)) {
            node[[i]] <- downApplyDen(node[[i]], FUN, node, depth + 1, tree, ...)
        }
    }
    node
}
#' @importFrom stats is.leaf
upApplyDen <- function(node, FUN, children = list(), depth = 0, tree = node, ...) {
    if (!is.leaf(node)) {
        for (i in seq_along(node)) {
            children <- node[[i]]
            class(children) <- 'list'
            attributes(children) <- NULL
            node[[i]] <- upApplyDen(node[[i]], FUN, children, depth + 1, tree, ...)
        }
    }
    children <- node
    class(children) <- 'list'
    attributes(children) <- NULL
    args <- list(node = node, children = children, depth = depth, tree = tree, ...)
    do.call(FUN, args)
}
