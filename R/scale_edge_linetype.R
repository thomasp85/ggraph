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
#' @export
scale_edge_linetype <- function(..., na.value = 'blank') {
  sc <- scale_linetype(..., na.value = na.value)
  sc$aesthetics <- 'edge_linetype'
  sc
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_continuous <- function(...) {
  cli::cli_abort(c(
    "A continuous variable cannot be mapped to the {.field edge_linetype} aesthetic",
    "i" = "choose a different aesthetic or use {.fn scale_edge_linetype_binned}"
  ))
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_discrete <- scale_edge_linetype
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_binned <- function(..., na.value = "blank") {
  sc <- scale_linetype_binned(..., na.value = na.value)
  sc$aesthetics <- 'edge_linetype'
  sc
}
#' @rdname scale_edge_linetype
#'
#' @inheritParams ggplot2::scale_linetype_manual
#'
#' @export
scale_edge_linetype_manual <- function(
  ...,
  values,
  breaks = waiver(),
  na.value = "blank"
) {
  sc <- scale_linetype_manual(
    ...,
    values = values,
    breaks = breaks,
    na.value = na.value
  )
  sc$aesthetics <- 'edge_linetype'
  sc
}
#' @rdname scale_edge_linetype
#'
#' @inheritParams ggplot2::scale_linetype_identity
#' @export
scale_edge_linetype_identity <- function(..., guide = 'none') {
  sc <- scale_linetype_identity(..., guide = guide)
  sc$aesthetics <- 'edge_linetype'
  sc
}
