#' Edge size scales
#'
#' This set of scales defines new size scales for edge geoms equivalent to the
#' ones already defined by ggplot2. See [ggplot2::scale_size()] for
#' more information. The different geoms will know whether to use edge scales or
#' the standard scales so it is not necessary to write `edge_size` in
#' the call to the geom - just use `size`.
#'
#' @note In ggplot2 size conflates both line width and point size into one
#' scale. In ggraph there is also a width scale ([scale_edge_width()])
#' that is used for linewidth. As edges are often represented by lines the width
#' scale is the most common.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_size
#' @rdname scale_edge_size
#'
NULL

#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_continuous
#'
#' @importFrom scales area_pal
#' @export
scale_edge_size_continuous <- function(..., range = c(1, 6)) {
  continuous_scale('edge_size', 'area', area_pal(range), ...)
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_radius
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_radius <- function(..., range = c(1, 6)) {
  continuous_scale('edge_size', 'radius', rescale_pal(range), ...)
}
#' @rdname scale_edge_size
#'
#' @export
scale_edge_size <- scale_edge_size_continuous
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_discrete
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_size_discrete <- function(..., range = c(2, 6)) {
  discrete_scale('edge_size', 'size_d', function(n) {
    area <- seq(range[1]^2, range[2]^2, length.out = n)
    sqrt(area)
  }, ...)
}
#' @rdname scale_edge_size
#'
#'
#' @inheritParams ggplot2::scale_size_area
#'
#' @importFrom scales abs_area rescale_max
#' @export
scale_edge_size_area <- function(..., max_size = 6) {
  continuous_scale('edge_size', 'area',
    palette = abs_area(max_size),
    rescaler = rescale_max, ...
  )
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_edge_size_manual <- function(..., values) {
  manual_scale('edge_size', values, ...)
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_size_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('edge_size', 'identity', identity_pal(), ...,
    guide = guide, super = ScaleDiscreteIdentity
  )
  sc
}
