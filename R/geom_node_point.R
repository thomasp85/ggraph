#' Show nodes as points
#'
#' This geom is equivalent in functionality to geom_point and allows for simple
#' plotting of nodes in different shapes, colours and sizes. For more control
#' and textbox-like plotting of nodes see \code{\link{geom_node_box}}.
#'
#' @section Aesthetics:
#' geom_node_point understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{alpha}
#'  \item{colour}
#'  \item{fill}
#'  \item{shape}
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
#' @param data The return of a call to \code{gEdges('short', ...)} or a
#' data.frame giving edges in the short format. See \code{\link{gEdges}} for
#' more details.
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
#' @importFrom ggplot2 GeomPoint aes_
#' @export
#'
geom_node_point <- function(mapping = NULL, data = NULL, position = "identity",
                            show.legend = NA, ...) {
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = GeomPoint,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, ...)
    )
}
