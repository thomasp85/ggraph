#' Create heirarchical edge bundles between nodes
#'
#' Hierarchical edge bundling is a technique to introduce some order into the
#' hairball structure that can appear when there's a lot of overplotting and
#' edge crossing in a network plot. The concept requires that the network has
#' an intrinsic hierarchical structure that defines the layout but is not shown.
#' Connections between points (that is, not edges) are then drawn so that they
#' loosely follows the underlying hierarchical structure. This results in a
#' flow-like structure where lines that partly move in the same direction will
#' be bundled togeether.
#'
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' \code{edge_colour} within the \code{aes()} call as \code{colour} will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_edge_bundle* understands the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{group}}
#'  \item{\strong{circular}}
#'  \item{edge_colour}
#'  \item{edge_width}
#'  \item{edge_linetype}
#'  \item{edge_alpha}
#'  \item{filter}
#' }
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams ggplot2::geom_path
#'
#' @param data The result of a call to \code{\link{gCon}}
#'
#' @param n The number of points to calculate along the edge. Defaults to 100
#'
#' @param tension How "loose" should the bundles be. 1 will give very tight
#' bundles, while 0 will turn of bundling completely and give straight lines.
#' Defaults to 0.8
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @references
#' Holten, D. (2006). \emph{Hierarchical edge bundles: visualization
#' of adjacency relations in hierarchical data.} IEEE Transactions on
#' Visualization and Computer Graphics, \strong{12}(5), 741-748.
#' http://doi.org/10.1109/TVCG.2006.147
#'
#' @examples
#' # Create a graph of the flare class system
#' library(igraph)
#' flareGraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#' importFrom <- match(flare$imports$from, flare$vertices$name)
#' importTo <- match(flare$imports$to, flare$vertices$name)
#' flareGraph <- treeApply(flareGraph, function(node, parent, depth, tree) {
#'     tree <- set_vertex_attr(tree, 'depth', node, depth)
#'     if (depth == 1) {
#'         tree <- set_vertex_attr(tree, 'class', node, V(tree)$shortName[node])
#'     } else if (depth > 1) {
#'         tree <- set_vertex_attr(tree, 'class', node, V(tree)$class[parent])
#'     }
#'     tree
#' })
#' V(flareGraph)$leaf <- degree(flareGraph, mode = 'out') == 0
#'
#' # Use class inheritance for layout but plot class imports as bundles
#' ggraph(flareGraph, 'dendrogram', circular = TRUE) +
#'     geom_edge_bundle(aes(colour = ..index..), data = gCon(importFrom, importTo),
#'                      edge_alpha = 0.25) +
#'     geom_node_point(aes(filter = leaf, colour = class)) +
#'     scale_edge_colour_distiller('', direction = 1, guide = 'edge_direction') +
#'     coord_fixed() +
#'     ggforce::theme_no_axes()
#'
#' @rdname geom_edge_bundle
#' @name geom_edge_bundle
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @importFrom ggplot2 ggproto
#' @export
StatEdgeBundle <- ggproto('StatEdgeBundle', StatBspline,
    setup_data = function(data, params) {
        if (any(names(data) == 'filter')) {
            if (!is.logical(data$filter)) {
                stop('filter must be logical')
            }
            data <- data[data$filter, names(data) != 'filter']
        }
        relax(data, params$tension)
    },
    setup_params = function(data, params) {
        if (is.null(params$tension)) {
            params$tension <- 0.8
        }
        if (params$tension < 0) params$tension <- 0
        if (params$tension > 1) params$tension <- 1
        params
    },
    required_aes = c('x', 'y'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'tension')
)
#' @rdname geom_edge_bundle
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_bundle <- function(mapping = NULL, data = gCon(),
                             position = "identity", arrow = NULL,
                             lineend = "butt", show.legend = NA,
                             n = 100, tension = 0.8, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~con.id))
    layer(data = data, mapping = mapping, stat = StatEdgeBundle,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = FALSE, tension = tension, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline2
#' @importFrom ggplot2 ggproto
#' @export
StatEdgeBundle2 <- ggproto('StatEdgeBundle2', StatBspline2,
    setup_data = function(data, params) {
        StatEdgeBundle$setup_data(data, params)
    },
    setup_params = function(data, params) {
        StatEdgeBundle$setup_params(data, params)
    },
    required_aes = c('x', 'y'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'tension')
)
#' @rdname geom_edge_bundle
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_bundle2 <- function(mapping = NULL, data = gCon(),
                              position = "identity", arrow = NULL,
                              lineend = "butt", show.legend = NA,
                              n = 100, tension = 0.8, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~con.id))
    layer(data = data, mapping = mapping, stat = StatEdgeBundle2,
          geom = GeomEdgePath, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
                        interpolate = TRUE, tension = tension, ...)
    )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
StatEdgeBundle0 <- ggproto('StatEdgeBundle0', StatIdentity,
    setup_data = function(data, params) {
        StatEdgeBundle$setup_data(data, params)
    },
    setup_params = function(data, params) {
        StatEdgeBundle$setup_params(data, params)
    },
    required_aes = c('x', 'y'),
    default_aes = aes(filter = TRUE),
    extra_params = c('na.rm', 'n', 'tension')
)
#' @rdname geom_edge_bundle
#'
#' @importFrom ggplot2 layer aes_
#' @export
geom_edge_bundle0 <- function(mapping = NULL, data = gCon(),
                              position = "identity", arrow = NULL,
                              lineend = "butt", show.legend = NA,
                              tension = 0.8, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~con.id))
    layer(data = data, mapping = mapping, stat = StatEdgeBundle0,
          geom = GeomEdgeBspline, position = position, show.legend = show.legend,
          inherit.aes = FALSE,
          params = list(arrow = arrow, lineend = lineend, na.rm = FALSE,
                        tension = tension, ...)
    )
}
#' @importFrom utils head tail
relax <- function(data, strength) {
    formula <- function(p, startInd, endInd, pathLengths) {
        start <- rep(p[startInd], pathLengths)
        range <- rep(p[endInd] - p[startInd], pathLengths)
        ind <- unlist(lapply(pathLengths, seq_len)) - 1
        length <- rep(pathLengths, pathLengths)
        strength*p + (1 - strength)*(start + (ind/(length - 1))*range)
    }
    idInds <- split(seq_len(nrow(data)), data$group)
    pathLengths <- lengths(idInds)
    startInd <- sapply(idInds, head, n = 1)
    endInd <- sapply(idInds, tail, n = 1)
    data$x <- formula(data$x, startInd, endInd, pathLengths)
    data$y <- formula(data$y, startInd, endInd, pathLengths)
    data
}
