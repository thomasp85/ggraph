#' Draw edges as glyphs
#'
#' This geom draws edges as glyphs with their x-position defined by the
#' x-position of the start node, and the y-position defined by the y-position of
#' the end node. As such it will result in a matrix layout when used in
#' conjunction with \code{\link{layout_tbl_graph_matrix}}
#'
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' \code{edge_colour} within the \code{aes()} call as \code{colour} will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_edge_point understands the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{edge_shape}
#'  \item{edge_colour}
#'  \item{edge_size}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. By default x, y, xend, yend, group and
#' circular are mapped to x, y, xend, yend, edge.id and circular in the edge
#' data.
#'
#' @param data The return of a call to \code{get_edges()} or a data.frame
#' giving edges in corrent format (see details for for guidance on the format).
#' See \code{\link{get_edges}} for more details on edge extraction.
#'
#' @param mirror Logical. Should edge points be duplicated on both sides of the
#' diagonal. Intended for undirected graphs. Default to \code{FALSE}
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_link(aes(alpha = ..index..))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_link2(aes(colour = node.class))
#'
#' ggraph(gr, 'igraph', algorithm = 'nicely') +
#'   geom_edge_link0(aes(colour = class))
#'
#' @rdname geom_edge_point
#' @name geom_edge_point
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('zachary') %>%
#'   mutate(group = group_infomap()) %>%
#'   morph(to_split, group) %>%
#'   activate(edges) %>%
#'   mutate(edge_group = as.character(.N()$group[1])) %>%
#'   unmorph()
#'
#' ggraph(gr, 'matrix', sort.by = node_rank_hclust()) +
#'   geom_edge_point(aes(colour = edge_group), mirror = TRUE, edge_size = 3) +
#'   scale_y_reverse() +
#'   coord_fixed() +
#'   theme_graph() +
#'   labs(edge_colour = 'Infomap Cluster') +
#'   ggtitle("Zachary' Karate Club")
#'
NULL

#' @rdname geom_edge_point
#'
#' @export
geom_edge_point <- function(mapping = NULL, data = get_edges(),
                            position = "identity", mirror = FALSE,
                            show.legend = NA, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~yend))
    layer(data = data, mapping = mapping, stat = StatFilter, geom = GeomEdgePoint,
          position = position, show.legend = show.legend, inherit.aes = FALSE,
          params = list(na.rm = FALSE, mirror = mirror, ...)
    )
}
