#' Edge linetype scales
#'
#' This set of scales defines new linetype scales for edge geoms equivalent to
#' the ones already defined by ggplot2. See
#' [ggplot2::scale_linetype()] for more information. The different
#' geoms will know whether to use edge scales or the standard scales so it is
#' not necessary to write `edge_linetype` in the call to the geom - just
#' use `linetype`.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_linetype
#' @rdname scale_edge_linetype
#'
NULL

#' @rdname scale_edge_linetype
#'
#' @inheritParams ggplot2::scale_linetype
#'
#' @importFrom scales linetype_pal
#' @export
scale_edge_linetype <- function(..., na.value = 'blank') {
  discrete_scale('edge_linetype', 'linetype_d', linetype_pal(),
    na.value = na.value, ...
  )
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_continuous <- function(...) {
  stop('A continuous variable can not be mapped to linetype', call. = FALSE)
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_discrete <- scale_edge_linetype
#' @rdname scale_edge_linetype
#'
#' @inheritParams ggplot2::scale_linetype_manual
#'
#' @export
scale_edge_linetype_manual <- function(..., values) {
  manual_scale('edge_linetype', values, ...)
}
#' @rdname scale_edge_linetype
#'
#' @inheritParams ggplot2::scale_linetype_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_linetype_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('edge_linetype', 'identity', identity_pal(), ...,
    guide = guide, super = ScaleDiscreteIdentity
  )
  sc
}
