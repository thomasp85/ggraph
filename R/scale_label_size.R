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
#' @export
scale_label_size_continuous <- function(
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
  sc$aesthetics <- 'label_size'
  sc
}
#' @rdname scale_label_size
#'
#' @export
scale_label_size <- scale_label_size_continuous
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_discrete
#'
#' @export
scale_label_size_discrete <- function(...) {
  cli::cli_warn(
    "Using {.field label_size} for a discrete variable is not advised."
  )
  sc <- scale_size_ordinal(...)
  sc$aesthetics <- 'label_size'
  sc
}
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_binned
#' @export
scale_label_size_binned <- function(
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
  sc$aesthetics <- 'label_size'
  sc
}
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_label_size_manual <- function(
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
  sc$aesthetics <- 'label_size'
  sc
}
#' @rdname scale_label_size
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_label_size_identity <- function(..., guide = 'none') {
  sc <- scale_size_identity(..., guide = guide)
  sc$aesthetics <- 'label_size'
  sc
}
