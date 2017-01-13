#' Edge width scales
#'
#' This set of scales defines width scales for edge geoms. Of all the new edge
#' scales defined in ggraph, this is the only one not having an equivalent in
#' ggplot2. In essence it mimicks the use of size in
#' \code{\link[ggplot2]{geom_line}} and related. As almost all edge
#' representations are lines of some sort, edge_width will be used much more
#' often than edge_size. It is not necessary to spell out that it is an edge
#' scale as the geom knows if it is drawing an edge. Just write \code{width} and
#' not \code{edge_width} in the call to geoms.
#'
#' @param ... Other arguments passed on to
#' \code{\link[ggplot2]{continuous_scale}} or
#' \code{\link[ggplot2]{discrete_scale}} as appropriate, to control name,
#' limits, breaks, labels and so forth.
#'
#' @param range Range of output width values.
#'
#' @param values A set of aesthetic values to map data values to. If this is a
#' named vector, then the values will be matched based on the names. If unnamed,
#' values will be matched in order (usually alphabetical) with the limits of the
#' scale. Any data values that don't match will be given na.value.
#'
#' @param guide Guide to use for this scale.
#'
#' @return A ggproto object inheriting from \code{Scale}
#'
#' @family scale_edge_*
#'
#' @name scale_edge_width
#' @rdname scale_edge_width
#'
NULL

#' @rdname scale_edge_width
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_continuous <- function(..., range = c(1, 6)) {
    continuous_scale("edge_width", "width_c", rescale_pal(range), ...)
}
#' @rdname scale_edge_width
#'
#' @export
scale_edge_width <- scale_edge_width_continuous
#' @rdname scale_edge_width
#'
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_discrete <- function(..., range = c(2, 6)) {
    discrete_scale("edge_width", "width_d", function(n) {
        area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
        sqrt(area)
    }, ...)
}
#' @rdname scale_edge_width
#'
#' @export
scale_edge_width_manual <- function(..., values) {
    manual_scale("edge_width", values, ...)
}
#' @rdname scale_edge_width
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_width_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_width", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
