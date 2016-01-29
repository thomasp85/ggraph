#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 aes
#' @export
#'
GeomTreemap <- ggproto('GeomTreemap', GeomTile,
    default_aes = aes(fill = NA, colour = 'black', size = 0.5, linetype = 1,
                      alpha = NA)
)

#' Show nodes as points
#'
#' A treemap is a space filling layout that recursively divides a rectangle to
#' the children of the node. Often only the leaf nodes are drawn as nodes higher
#' up in the hierarchy would obscure what is below.
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
#' gr <- make_graph('bull')
#' V(gr)$class <- sample(letters[1:3], gorder(gr), replace = TRUE)
#'
#' ggraph(gr, 'igraph', type = 'nicely') + geom_node_point()
#'
#' @importFrom ggplot2 GeomTile aes_
#' @export
#'
geom_treemap <- function(mapping = NULL, data = NULL, position = "identity",
                         show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, width=~width,
                                          height=~height))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = GeomTreemap,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, ...)
    )
}
