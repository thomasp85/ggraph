#' @export
treeApply <- function(tree, FUN, ...) {
    UseMethod('treeApply')
}
#' @export
treeApply.default <- function(tree, FUN, ...) {
    stop('treeApply not implemented for ', class(tree), ' objects')
}
#' @importFrom igraph bfs
#' @export
treeApply.igraph <- function(tree, FUN, direction = 'down', mode = 'out', ...) {
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
