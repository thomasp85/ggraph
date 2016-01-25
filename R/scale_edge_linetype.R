#' Edge linetype scales
#'
#' This set of scales defines new linetype scales for edge geoms equivalent to
#' the ones already defined by ggplot2. See
#' \code{\link[ggplot2]{scale_linetype}} for more information. The different
#' geoms will know whether to use edge scales or the standard scales so it is
#' not necessary to write \code{edge_linetype} in the call to the geom - just
#' use \code{linetype}.
#'
#' @param ... Other arguments passed on to
#' \code{\link[ggplot2]{discrete_scale}} as appropriate, to control name,
#' limits, breaks, labels and so forth.
#'
#' @param na.value The linetype to use for NA values.
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
#' @name scale_edge_linetype
#' @rdname scale_edge_linetype
#'
NULL

#' @rdname scale_edge_linetype
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales linetype_pal
#' @export
scale_edge_linetype <- function(..., na.value = "blank") {
    discrete_scale("edge_linetype", "linetype_d", linetype_pal(),
                   na.value = na.value, ...)
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_continuous <- function(...) {
    stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_discrete <- scale_edge_linetype
#' @rdname scale_edge_linetype
#'
#' @export
scale_edge_linetype_manual <- function(..., values) {
    manual_scale("edge_linetype", values, ...)
}
#' @rdname scale_edge_linetype
#'
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_linetype_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_linetype", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
