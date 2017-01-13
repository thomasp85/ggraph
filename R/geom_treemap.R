#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
#'
GeomTreemap <- ggproto('GeomTreemap', GeomTile,
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
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x, y, xend and yend are mapped to
#' x, y, xend and yend in the edge data. Thus only edge_fill is really relevant
#' to set.
#'
#' @param data A data frame. If specified, overrides the default data frame
#' defined at the top level of the plot.
#'
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function. Currently no meaningful position
#' adjustment exists for edges.
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#' are three types of arguments you can use here:
#' \itemize{
#'  \item{Aesthetics: to set an aesthetic to a fixed value, like
#'  \code{color = "red"} or \code{size = 3.}}
#'  \item{Other arguments to the layer, for example you override the default
#'  \code{stat} associated with the layer.}
#'  \item{Other arguments passed on to the stat.}
#' }
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#' never includes, and \code{TRUE} always includes.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(igraph)
#' gr <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#'
#' ggraph(gr, 'treemap', weight = 'size') + geom_treemap()
#'
#' # We can color by modifying the graph
#' gr <- treeApply(gr, function(node, parent, depth, tree) {
#'   tree <- set_vertex_attr(tree, 'depth', node, depth)
#'   if (depth == 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$shortName[node])
#'   } else if (depth > 1) {
#'     tree <- set_vertex_attr(tree, 'Class', node, V(tree)$Class[parent])
#'   }
#'   tree
#' })
#'
#' ggraph(gr, 'treemap', weight = 'size') +
#'   geom_treemap(aes(fill = Class, filter = leaf, alpha = depth), colour = NA) +
#'   geom_treemap(aes(size = depth), colour = 'white') +
#'   scale_alpha(range = c(1, 0.5), guide = 'none') +
#'   scale_size(range = c(4, 0.2), guide = 'none')
#'
#' @export
#'
geom_node_treemap <- function(mapping = NULL, data = NULL, position = "identity",
                         show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, width=~width,
                                          height=~height))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = GeomTreemap,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, ...)
    )
}
#' @rdname geom_node_treemap
#' @export
geom_treemap <- geom_node_treemap
