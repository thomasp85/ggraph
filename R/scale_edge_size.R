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
#' @export
scale_edge_size_continuous <- function(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  range = c(1, 6),
  trans = "identity",
  guide = "legend"
) {
  sc <- scale_size_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    range = range,
    trans = trans,
    guide = guide
  )
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_radius
#'
#' @export
scale_edge_radius <- function(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  range = c(1, 6),
  trans = "identity",
  guide = "legend"
) {
  sc <- scale_radius(
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    range = range,
    trans = trans,
    guide = guide
  )
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @export
scale_edge_size <- scale_edge_size_continuous
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_edge_size_discrete <- function(...) {
  cli::cli_warn(
    "Using {.field edge_size} for a discrete variable is not advised."
  )
  sc <- scale_size_ordinal(...)
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_binned
#' @export
scale_edge_size_binned <- function(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  range = c(1, 6),
  n.breaks = NULL,
  nice.breaks = TRUE,
  trans = "identity",
  guide = "bins"
) {
  sc <- scale_size_binned(
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    range = range,
    n.breaks = n.breaks,
    nice.breaks = nice.breaks,
    trans = trans,
    guide = guide
  )
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_area
#'
#' @export
scale_edge_size_area <- function(..., max_size = 6) {
  sc <- scale_size_area(..., max_size = max_size)
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#'
#' @inheritParams ggplot2::scale_size_binned_area
#'
#' @export
scale_edge_size_binned_area <- function(..., max_size = 6) {
  sc <- scale_size_binned_area(..., max_size = max_size)
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_edge_size_manual <- function(
  ...,
  values,
  breaks = waiver(),
  na.value = NA
) {
  sc <- scale_size_manual(
    ...,
    values = values,
    breaks = breaks,
    na.value = na.value
  )
  sc$aesthetics <- 'edge_size'
  sc
}
#' @rdname scale_edge_size
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @export
scale_edge_size_identity <- function(..., guide = 'none') {
  sc <- scale_size_identity(..., guide = guide)
  sc$aesthetics <- 'edge_size'
  sc
}
