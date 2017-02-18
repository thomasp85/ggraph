#' Apply a function recursively to a tree
#'
#' This function allows for easy recursive function calling of tree-like
#' structures. The recursion can happen either downwards, calling the children
#' after the parent, or upwards, calling the parent after the children. At each
#' recursion there is access to the node(s) that was/were called before and thus
#' have already been through the function.
#'
#' @details The function is called with a set of predifined parameters along
#' with user defined ones. If \code{direction = 'down'} The parameters supplied
#' automatically to the function are: \code{node}, \code{parent}, \code{depth}
#' and \code{tree}, while if \code{direction = 'up'} the parameters are:
#' \code{node}, \code{children}, \code{depth} and \code{tree}. The nature of
#' \code{node}, \code{parent} and \code{children} depends on the class of
#' \code{tree}. If \code{class(tree) == 'igraph'} they will be indices of the
#' relevant vertices in the graph. If \code{class(tree) == 'dendrogram'} they
#' will be the actual dendrogram objects.
#'
#' @param tree A tree-like object. Currently support for igraph objects
#'
#' @param FUN The function to apply to each node. The function must return a
#' modified version of the graph if \code{class(tree) == 'igraph'} or a modified
#' version of the node if \code{class(tree) == 'dendrogram'}
#'
#' @param direction The direction of the recursion. If \code{direction = 'down'}
#' The parent will get handled before the children, while the reverse is true if
#' \code{direction = 'up'}
#'
#' @param mode For \code{class(tree) == 'igraph'} the directionality of the
#' edges in the graph. If \code{mode = 'out'} then parents points towards their
#' children, while the reverse is true for \code{mode = 'in'}
#'
#' @param ... Additional parameters to \code{FUN}
#'
#' @return A modified version of \code{tree} (if anything is modified in
#' \code{FUN})
#'
#' @examples
#' # We'll start with igraph
#' require(igraph)
#' gr <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#'
#' # Set depth and a class based on the name of the 2nd level node name
#' gr <- tree_apply(gr, function(node, parent, depth, tree) {
#'   tree <- set_vertex_attr(tree, 'depth', node, depth)
#'   if (depth == 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$shortName[node])
#'   } else if (depth > 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$Class[parent])
#'   }
#'   tree
#' })
#'
#' # For dendrograms it's slightly different
#' irisDen <- as.dendrogram(hclust(dist(iris[1:4], method='euclidean'),
#'                          method='ward.D2'))
#' # Add the species information to the leafs
#' irisDen <- dendrapply(irisDen, function(d) {
#'   if(is.leaf(d))
#'     attr(d, 'nodePar') <- list(species=iris[as.integer(attr(d, 'label')),5])
#'   d
#' })
#'
#' # Set class of node to the class of it's children they are of equal class
#' irisDen <- tree_apply(irisDen, function(node, children, ...) {
#'   if (is.leaf(node)) {
#'     attr(node, 'Class') <- attr(node, 'nodePar')$species
#'   } else {
#'     classes <- unique(sapply(children, attr, which = 'Class'))
#'     if (length(classes) == 1 && !anyNA(classes)) {
#'       attr(node, 'Class') <- classes
#'     } else {
#'       attr(node, 'Class') <- NA
#'     }
#'   }
#'   node
#' }, direction = 'up')
#'
#' @export
#'
tree_apply <- function(tree, FUN, ...) {
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
