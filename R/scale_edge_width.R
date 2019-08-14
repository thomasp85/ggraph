#' Edge width scales
#'
#' This set of scales defines width scales for edge geoms. Of all the new edge
#' scales defined in ggraph, this is the only one not having an equivalent in
#' ggplot2. In essence it mimics the use of size in
#' [ggplot2::geom_line()] and related. As almost all edge
#' representations are lines of some sort, edge_width will be used much more
#' often than edge_size. It is not necessary to spell out that it is an edge
#' scale as the geom knows if it is drawing an edge. Just write `width` and
#' not `edge_width` in the call to geoms.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_width
#' @rdname scale_edge_width
#'
NULL

#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_continuous
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_continuous <- function(..., range = c(1, 6)) {
  continuous_scale('edge_width', 'width_c', rescale_pal(range), ...)
}
#' @rdname scale_edge_width
#'
#' @export
scale_edge_width <- scale_edge_width_continuous
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_discrete
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_discrete <- function(..., range = c(2, 6)) {
  discrete_scale('edge_width', 'width_d', function(n) {
    area <- seq(range[1]^2, range[2]^2, length.out = n)
    sqrt(area)
  }, ...)
}
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_edge_width_manual <- function(..., values) {
  manual_scale('edge_width', values, ...)
}
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_width_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('edge_width', 'identity', identity_pal(), ...,
    guide = guide, super = ScaleDiscreteIdentity
  )
  sc
}
