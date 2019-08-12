#' Edge label size scales
#'
#' This set of scales defines new size scales for edge labels in order to allow
#' for separate sizing of edges and their labels.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_label_size
#' @rdname scale_label_size
#'
NULL

#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_continuous
#'
#' @importFrom scales area_pal
#' @export
scale_label_size_continuous <- function(..., range = c(1, 6)) {
  continuous_scale('label_size', 'area', area_pal(range), ...)
}
#' @rdname scale_label_size
#'
#' @export
scale_label_size <- scale_label_size_continuous
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_discrete
#'
#' @importFrom scales rescale_pal
#' @export
scale_label_size_discrete <- function(..., range = c(2, 6)) {
  discrete_scale('label_size', 'size_d', function(n) {
    area <- seq(range[1]^2, range[2]^2, length.out = n)
    sqrt(area)
  }, ...)
}
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_label_size_manual <- function(..., values) {
  manual_scale('label_size', values, ...)
}
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_label_size_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('label_size', 'identity', identity_pal(), ...,
    guide = guide, super = ScaleDiscreteIdentity
  )
  sc
}
