#' Annotate nodes with text
#'
#' These geoms are equivalent in functionality to geom_text and geom_label and
#' allows for simple annotation of nodes.
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
#' @inheritParams ggplot2::geom_text
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x and y are mapped to x and y in
#' the node data.
#'
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#' Useful for offsetting text from points, particularly on discrete scales.
#'
#' @param repel If \code{TRUE}, text labels will be repelled from each other
#' to avoid overlapping, using the \code{GeomTextRepel} geom from the
#' ggrepel package.
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
#' ggraph(gr, 'igraph', algorithm = 'nicely') + geom_node_label(aes(label = class))
#'
#' @importFrom ggrepel GeomTextRepel
#' @export
#'
geom_node_text <- function(mapping = NULL, data = NULL, position = "identity",
                           parse = FALSE, nudge_x = 0, nudge_y = 0,
                           check_overlap = FALSE, show.legend = NA,
                           repel = FALSE, ...) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`",
                 call. = FALSE)
        }
        position <- position_nudge(nudge_x, nudge_y)
    }
    params <- list(parse = parse, na.rm = FALSE, ...)
    if (repel) {
      geom <- GeomTextRepel
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
#' @rdname geom_node_text
#'
#' @inheritParams ggplot2::geom_label
#'
#' @importFrom ggrepel GeomLabelRepel
#' @export
#'
geom_node_label <- function(mapping = NULL, data = NULL, position = "identity",
                           parse = FALSE, nudge_x = 0, nudge_y = 0,
                           label.padding = unit(0.25, "lines"),
                           label.r = unit(0.15, "lines"),
                           label.size = 0.25, show.legend = NA,
                           repel = FALSE, ...) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`",
                 call. = FALSE)
        }
        position <- position_nudge(nudge_x, nudge_y)
    }
    params <- list(parse = parse, label.padding = label.padding,
                   label.r = label.r, label.size = label.size, na.rm = FALSE, ...)
    if (repel) {
        geom <- GeomLabelRepel
    } else {
        geom <- GeomLabel
    }

    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = geom,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = params
    )
}
