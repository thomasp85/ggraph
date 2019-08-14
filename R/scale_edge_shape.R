#' Edge shape scales
#'
#' This set of scales defines new shape scales for edge geoms equivalent to the
#' ones already defined by ggplot2. See [ggplot2::scale_shape()] for
#' more information. The different geoms will know whether to use edge scales or
#' the standard scales so it is not necessary to write `edge_shape` in
#' the call to the geom - just use `shape`.
#'
#' @param guide Guide to use for this scale.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_shape
#' @rdname scale_edge_shape
#'
NULL

#' @rdname scale_edge_shape
#'
#' @inheritParams ggplot2::scale_shape
#'
#' @importFrom scales shape_pal
#' @export
scale_edge_shape <- function(..., solid = TRUE) {
  discrete_scale('edge_shape', 'shape_d', shape_pal(solid), ...)
}
#' @rdname scale_edge_shape
#'
#' @export
scale_edge_shape_discrete <- scale_edge_shape
#' @rdname scale_edge_shape
#'
#' @export
scale_edge_shape_continuous <- function(...) {
  stop('A continuous variable can not be mapped to shape', call. = FALSE)
}
#' @rdname scale_edge_shape
#'
#' @inheritParams ggplot2::scale_shape_manual
#'
#' @export
scale_edge_shape_manual <- function(..., values) {
  manual_scale('edge_shape', values, ...)
}
#' @rdname scale_edge_shape
#'
#' @inheritParams ggplot2::scale_shape_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_shape_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('edge_shape', 'identity', identity_pal(), ...,
    guide = guide, ScaleDiscreteIdentity
  )
  sc
}
