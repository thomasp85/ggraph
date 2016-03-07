#' Show nodes as points
#'
#' This geom is equivalent in functionality to geom_point and allows for simple
#' plotting of nodes in different shapes, colours and sizes. For more control
#' and textbox-like plotting of nodes see \code{\link{geom_node_box}}.
#'
#' @section Aesthetics:
#' geom_node_point understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden. Italic aesthetics are required but
#' not set by default
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\emph{label}}
#'  \item{alpha}
#'  \item{angle}
#'  \item{colour}
#'  \item{family}
#'  \item{fontface}
#'  \item{hjust}
#'  \item{lineheight}
#'  \item{size}
#'  \item{vjust}
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
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#' displayed as described in ?plotmath
#'
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#' Useful for offsetting text from points, particularly on discrete scales.
#'
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#' same layer will not be plotted. Ignored if repel = TRUE. A quick and dirty way.
#' 
#' @param repel If \code{TRUE}, text labels will be repelled from each other
#' to avoid overlapping, using the \code{GeomTextRepel} geom from the
#' ggrepel package.
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
#' ggraph(gr, 'igraph', algorithm = 'nicely') + geom_node_point(aes(label = class))
#'
#' @importFrom ggplot2 GeomText aes_
#' @export
#'
geom_node_text <- function(mapping = NULL, data = NULL, position = "identity",
                           parse = FALSE, nudge_x = 0, nudge_y = 0,
                           check_overlap = FALSE, show.legend = NA,
                           repel = FALSE, ...) {
    params <- list(parse = parse, na.rm = FALSE, ...)
    if (repel) {
      geom <- ggrepel::GeomTextRepel
    } else {
      geom <- GeomText
      params$check_overlap <- check_overlap
    }
  
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = geom,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = params
    )
}
