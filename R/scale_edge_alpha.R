#' @importFrom ggplot2 continuous_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_alpha <- function(..., range = c(0.1, 1)) {
    continuous_scale("edge_alpha", "alpha_c", rescale_pal(range), ...)
}

#' @export
scale_edge_alpha_continuous <- scale_edge_alpha

#' @importFrom ggplot2 discrete_scale
#' @export
scale_edge_alpha_discrete <- function(..., range = c(0.1, 1)) {
    discrete_scale("edge_alpha", "alpha_d",
                   function(n) seq(range[1], range[2], length.out = n), ...)
}
#' @export
scale_edge_alpha_manual <- function(..., values) {
    manual_scale("edge_alpha", values, ...)
}

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
