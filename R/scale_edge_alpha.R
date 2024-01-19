#' Edge alpha scales
#'
#' This set of scales defines new alpha scales for edge geoms equivalent to the
#' ones already defined by ggplot2. See [ggplot2::scale_alpha()] for
#' more information. The different geoms will know whether to use edge scales or
#' the standard scales so it is not necessary to write `edge_alpha` in
#' the call to the geom - just use `alpha`.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_alpha
#' @rdname scale_edge_alpha
#'
NULL

#' @rdname scale_edge_alpha
#'
#' @inheritParams ggplot2::scale_alpha
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_alpha <- function(..., range = c(0.1, 1)) {
  sc <- scale_alpha(..., range = range)
  sc$aesthetics <- 'edge_alpha'
  sc
}

#' @rdname scale_edge_alpha
#'
#' @export
scale_edge_alpha_continuous <- scale_edge_alpha

#' @rdname scale_edge_alpha
#'
#'
#' @inheritParams ggplot2::scale_alpha_discrete
#'
#' @export
scale_edge_alpha_discrete <- function(..., range = c(0.1, 1)) {
  sc <- scale_alpha_discrete(..., range = range)
  sc$aesthetics <- 'edge_alpha'
  sc
}

#' @rdname scale_edge_alpha
#'
#'
#' @inheritParams ggplot2::scale_alpha_binned
#'
#' @export
scale_edge_alpha_binned <- function(..., range = c(0.1, 1)) {
  sc <- scale_alpha_binned(..., range = range)
  sc$aesthetics <- 'edge_alpha'
  sc
}

#' @rdname scale_edge_alpha
#'
#' @inheritParams ggplot2::scale_alpha_manual
#'
#' @export
scale_edge_alpha_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  sc <- scale_alpha_manual(..., values = values, breaks = breaks, na.value = na.value)
  sc$aesthetics <- 'edge_alpha'
  sc
}

#' @rdname scale_edge_alpha
#'
#' @inheritParams ggplot2::scale_alpha_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_alpha_identity <- function(..., guide = 'none') {
  sc <- scale_alpha_identity(..., guide = guide)
  sc$aesthetics <- 'edge_alpha'
  sc
}
