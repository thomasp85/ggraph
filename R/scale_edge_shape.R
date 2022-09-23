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
#' @export
scale_edge_shape <- function(..., solid = TRUE) {
  sc <- scale_shape(..., solid = solid)
  sc$aesthetics <- 'edge_shape'
  sc
}
#' @rdname scale_edge_shape
#'
#' @export
scale_edge_shape_discrete <- scale_edge_shape
#' @rdname scale_edge_shape
#'
#' @export
scale_edge_shape_continuous <- function(...) {
  cli::cli_abort(c(
    "A continuous variable cannot be mapped to the {.field edge_shape} aesthetic",
    "i" = "choose a different aesthetic or use {.fn scale_edge_shape_binned}"
  ))
}
#' @rdname scale_edge_shape
#'
#' @export
scale_edge_shape_binned <- function(..., solid = TRUE) {
  sc <- scale_shape_binned(..., solid = solid)
  sc$aesthetics <- 'edge_shape'
  sc
}
#' @rdname scale_edge_shape
#'
#' @inheritParams ggplot2::scale_shape_manual
#'
#' @export
scale_edge_shape_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  sc <- scale_shape_manual(..., values = values, breaks = breaks, na.value = na.value)
  sc$aesthetics <- 'edge_shape'
  sc
}
#' @rdname scale_edge_shape
#'
#' @inheritParams ggplot2::scale_shape_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_shape_identity <- function(..., guide = 'none') {
  sc <- scale_shape_identity(..., guide = guide)
  sc$aesthetics <- 'edge_shape'
  sc
}
