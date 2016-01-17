#' @importFrom ggplot2 waiver continuous_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                       limits = NULL, range = c(1, 6),
                                       trans = "identity", guide = "legend") {
    continuous_scale("edge_width", "width_c", rescale_pal(range), name = name,
                     breaks = breaks, labels = labels, limits = limits, trans = trans,
                     guide = guide)
}
#' @export
scale_edge_width <- scale_edge_width_continuous
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_width_discrete <- function(..., range = c(2, 6)) {
    discrete_scale("edge_width", "width_d", function(n) {
        area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
        sqrt(area)
    }, ...)
}
#' @export
scale_edge_width_manual <- function(..., values) {
    manual_scale("edge_width", values, ...)
}
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
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
