#' Create hierarchical edge bundles between node connections
#'
#' Hierarchical edge bundling is a technique to introduce some order into the
#' hairball structure that can appear when there's a lot of overplotting and
#' edge crossing in a network plot. The concept requires that the network has
#' an intrinsic hierarchical structure that defines the layout but is not shown.
#' Connections between points (that is, not edges) are then drawn so that they
#' loosely follows the underlying hierarchical structure. This results in a
#' flow-like structure where lines that partly move in the same direction will
#' be bundled together.
#'
#' @note In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' `edge_colour` within the `aes()` call as `colour` will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' geom_conn_bundle* understands the following aesthetics. Bold aesthetics are
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
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param data The result of a call to [get_con()]
#'
#' @param tension How "loose" should the bundles be. 1 will give very tight
#' bundles, while 0 will turn of bundling completely and give straight lines.
#' Defaults to 0.8
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_conn_*
#'
#' @references
#' Holten, D. (2006). *Hierarchical edge bundles: visualization
#' of adjacency relations in hierarchical data.* IEEE Transactions on
#' Visualization and Computer Graphics, **12**(5), 741-748.
#' <https://doi.org/10.1109/TVCG.2006.147>
#'
#' @examples
#' # Create a graph of the flare class system
#' library(tidygraph)
#' flareGraph <- tbl_graph(flare$vertices, flare$edges) %>%
#'   mutate(
#'     class = map_bfs_chr(node_is_root(), .f = function(node, dist, path, ...) {
#'       if (dist <= 1) {
#'         return(shortName[node])
#'       }
#'       path$result[[nrow(path)]]
#'     })
#'   )
#' importFrom <- match(flare$imports$from, flare$vertices$name)
#' importTo <- match(flare$imports$to, flare$vertices$name)
#'
#' # Use class inheritance for layout but plot class imports as bundles
#' ggraph(flareGraph, 'dendrogram', circular = TRUE) +
#'   geom_conn_bundle(aes(colour = stat(index)),
#'     data = get_con(importFrom, importTo),
#'     edge_alpha = 0.25
#'   ) +
#'   geom_node_point(aes(filter = leaf, colour = class)) +
#'   scale_edge_colour_distiller('', direction = 1, guide = 'edge_direction') +
#'   coord_fixed() +
#'   ggforce::theme_no_axes()
#' @rdname geom_conn_bundle
#' @name geom_conn_bundle
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline
#' @export
StatConnBundle <- ggproto('StatConnBundle', StatBspline,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
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
  extra_params = c('na.rm', 'n', 'tension', 'type')
)
#' @rdname geom_conn_bundle
#'
#' @export
geom_conn_bundle <- function(mapping = NULL, data = get_con(),
                             position = 'identity', arrow = NULL,
                             lineend = 'butt', show.legend = NA,
                             n = 100, tension = 0.8, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        group = con.id))
  layer(
    data = data, mapping = mapping, stat = StatConnBundle,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
        interpolate = FALSE, tension = tension, type = 'clamped', ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatBspline2
#' @export
StatConnBundle2 <- ggproto('StatConnBundle2', StatBspline2,
  setup_data = function(data, params) {
    StatConnBundle$setup_data(data, params)
  },
  setup_params = function(data, params) {
    StatConnBundle$setup_params(data, params)
  },
  required_aes = c('x', 'y'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'tension', 'type')
)
#' @rdname geom_conn_bundle
#'
#' @export
geom_conn_bundle2 <- function(mapping = NULL, data = get_con(),
                              position = 'identity', arrow = NULL,
                              lineend = 'butt', show.legend = NA,
                              n = 100, tension = 0.8, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        group = con.id))
  layer(
    data = data, mapping = mapping, stat = StatConnBundle2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE, n = n,
        interpolate = TRUE, tension = tension, type = 'clamped', ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatConnBundle0 <- ggproto('StatConnBundle0', StatIdentity,
  setup_data = function(data, params) {
    StatConnBundle$setup_data(data, params)
  },
  setup_params = function(data, params) {
    StatConnBundle$setup_params(data, params)
  },
  required_aes = c('x', 'y'),
  default_aes = aes(filter = TRUE),
  extra_params = c('na.rm', 'n', 'tension', 'type')
)
#' @rdname geom_conn_bundle
#'
#' @export
geom_conn_bundle0 <- function(mapping = NULL, data = get_con(),
                              position = 'identity', arrow = NULL,
                              lineend = 'butt', show.legend = NA,
                              tension = 0.8, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        group = con.id))
  layer(
    data = data, mapping = mapping, stat = StatConnBundle0,
    geom = GeomEdgeBspline, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        tension = tension, type = 'clamped', ...
      )
    )
  )
}
#' @importFrom utils head tail
relax <- function(data, strength) {
  formula <- function(p, startInd, endInd, pathLengths) {
    start <- rep(p[startInd], pathLengths)
    range <- rep(p[endInd] - p[startInd], pathLengths)
    ind <- unlist(lapply(pathLengths, seq_len)) - 1
    length <- rep(pathLengths, pathLengths)
    strength * p + (1 - strength) * (start + (ind / (length - 1)) * range)
  }
  idInds <- split(seq_len(nrow(data)), data$group)
  pathLengths <- lengths(idInds)
  startInd <- vapply(idInds, head, integer(1), n = 1)
  endInd <- vapply(idInds, tail, integer(1), n = 1)
  data$x <- formula(data$x, startInd, endInd, pathLengths)
  data$y <- formula(data$y, startInd, endInd, pathLengths)
  data
}
