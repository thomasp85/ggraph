#' Create edge bundles between nodes
#'
#' The concept of edge bundles...
#'
#' @references
#' Holten, D. (2006). \emph{Hierarchical edge bundles: visualization
#' of adjacency relations in hierarchical data.} IEEE Transactions on
#' Visualization and Computer Graphics, \strong{12}(5), 741-748.
#' http://doi.org/10.1109/TVCG.2006.147
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
