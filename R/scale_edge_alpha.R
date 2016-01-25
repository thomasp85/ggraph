#' Edge alpha scales
#'
#' This set of scales defines new alpha scales for edge geoms equivalent to the
#' ones already defined by ggplot2. See \code{\link[ggplot2]{scale_alpha}} for
#' more information. The different geoms will know whether to use edge scales or
#' the standard scales so it is not necessary to write \code{edge_alpha} in
#' the call to the geom - just use \code{alpha}.
#'
#' @param ... Other arguments passed on to
#' \code{\link[ggplot2]{continuous_scale}} or
#' \code{\link[ggplot2]{discrete_scale}} as appropriate, to control name,
#' limits, breaks, labels and so forth.
#'
#' @param range Range of output alpha values. Should lie between 0 and 1.
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
#' @name scale_edge_alpha
#' @rdname scale_edge_alpha
#'
NULL

#' @rdname scale_edge_alpha
#'
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_alpha <- function(..., range = c(0.1, 1)) {
    continuous_scale("edge_alpha", "alpha_c", rescale_pal(range), ...)
}

#' @rdname scale_edge_alpha
#'
#' @export
scale_edge_alpha_continuous <- scale_edge_alpha

#' @rdname scale_edge_alpha
#'
#' @importFrom ggplot2 discrete_scale
#' @export
scale_edge_alpha_discrete <- function(..., range = c(0.1, 1)) {
    discrete_scale("edge_alpha", "alpha_d",
                   function(n) seq(range[1], range[2], length.out = n), ...)
}
#' @rdname scale_edge_alpha
#'
#' @export
scale_edge_alpha_manual <- function(..., values) {
    manual_scale("edge_alpha", values, ...)
}

#' @rdname scale_edge_alpha
#'
#' @importFrom ggplot2 continuous_scale ScaleContinuousIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_alpha_identity <- function(..., guide = "none") {
    sc <- continuous_scale("edge_alpha", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleContinuousIdentity
    class(sc) <- class(ScaleContinuousIdentity)
    sc
}
