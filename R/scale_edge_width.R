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
#' @export
scale_edge_width_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                        limits = NULL, range = c(1, 6),
                                        trans = "identity", guide = "legend") {
  sc <- scale_radius(name = name, breaks = breaks, labels = labels, limits = limits,
                     range = range, trans = trans, guide = guide)
  sc$scale_name <- 'width_c'
  sc$aesthetics <- 'edge_width'
  sc
}
#' @rdname scale_edge_width
#'
#' @export
scale_edge_width <- scale_edge_width_continuous
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_edge_width_discrete <- function(...) {
  cli::cli_warn("Using {.field edge_width} for a discrete variable is not advised.")
  sc <- scale_size_ordinal(...)
  sc$scale_name <- 'width_d'
  sc$aesthetics <- 'edge_width'
  sc
}
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_binned
#' @export
scale_edge_width_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                    limits = NULL, range = c(1, 6), n.breaks = NULL,
                                    nice.breaks = TRUE, trans = "identity", guide = "bins") {
  sc <- scale_size_binned(name = name, breaks = breaks, labels = labels, limits = limits,
                          range = range, n.breaks = n.breaks, nice.breaks = nice.breaks,
                          trans = trans, guide = guide)
  sc$scale_name <- 'width_b'
  sc$aesthetics <- 'edge_width'
  sc
}
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_edge_width_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  sc <- scale_size_manual(..., values = values, breaks = breaks, na.value = na.value)
  sc$aesthetics <- 'edge_width'
  sc
}
#' @rdname scale_edge_width
#'
#' @inheritParams ggplot2::scale_size_identity
#'
#' @export
scale_edge_width_identity <- function(..., guide = 'none') {
  sc <- scale_size_identity(..., guide = guide)
  sc$aesthetics <- 'edge_width'
  sc
}
