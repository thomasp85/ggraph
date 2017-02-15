#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
#'
GeomNodeTile <- ggproto('GeomNodeTile', GeomTile,
    default_aes = aes(fill = NA, colour = 'black', size = 0.5, linetype = 1,
                      alpha = NA, width = 1, height = 1),
    required_aes = c('x', 'y')
)

#' Draw the rectangles in a treemap
#'
#' A treemap is a space filling layout that recursively divides a rectangle to
#' the children of the node. Often only the leaf nodes are drawn as nodes higher
#' up in the hierarchy would obscure what is below. \code{geom_treemap} is a
#' shorthand for \code{geom_node_treemap} as node is implicit in the case of
#' treemap drawing
#'
#' @section Aesthetics:
#' geom_treemap understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{width}}
#'  \item{\strong{height}}
#'  \item{alpha}
#'  \item{colour}
#'  \item{fill}
#'  \item{size}
#'  \item{stroke}
#'  \item{filter}
#' }
#'
#' @inheritParams ggplot2::geom_tile
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x, y, width and height are mapped to
#' x, y, width and height in the node data.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(igraph)
#' gr <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#'
#' ggraph(gr, 'treemap', weight = 'size') + geom_node_tile()
#'
#' # We can color by modifying the graph
#' gr <- tree_apply(gr, function(node, parent, depth, tree) {
#'   if (depth == 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$shortName[node])
#'   } else if (depth > 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$Class[parent])
#'   }
#'   tree
#' })
#'
#' ggraph(gr, 'treemap', weight = 'size') +
#'   geom_node_tile(aes(fill = Class, filter = leaf, alpha = depth), colour = NA) +
#'   geom_node_tile(aes(size = depth), colour = 'white') +
#'   scale_alpha(range = c(1, 0.5), guide = 'none') +
#'   scale_size(range = c(4, 0.2), guide = 'none')
#'
#' @export
#'
geom_node_tile <- function(mapping = NULL, data = NULL, position = "identity",
                         show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, width=~width,
                                          height=~height))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = GeomNodeTile,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, ...)
    )
}
#' @rdname geom_node_tile
#' @usage NULL
#' @export
geom_node_treemap <- function(mapping = NULL, data = NULL, position = "identity",
                              show.legend = NA, ...) {
    .Deprecated('geom_node_tile')
    geom_node_tile(mapping = mapping, data = data, position = position,
                   show.legend = show.legend, ...)
}
#' @rdname geom_node_tile
#' @usage NULL
#' @export
geom_treemap <- geom_node_treemap
