#' Show nodes as points
#'
#' This geom is equivalent in functionality to \code{\link[ggplot2]{geom_point}}
#' and allows for simple plotting of nodes in different shapes, colours and sizes.
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
#' @inheritParams ggplot2::geom_point
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x and y are mapped to x and y in
#' the node data.
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
#' ggraph(gr, 'igraph', algorithm = 'nicely') + geom_node_point()
#'
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
