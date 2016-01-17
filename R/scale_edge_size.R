#' @importFrom ggplot2 waiver continuous_scale
#' @importFrom scales area_pal
#' @export
scale_edge_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend") {
    continuous_scale("edge_size", "area", area_pal(range), name = name,
                     breaks = breaks, labels = labels, limits = limits, trans = trans,
                     guide = guide)
}
#' @importFrom ggplot2 waiver continuous_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         trans = "identity", guide = "legend") {
    continuous_scale("edge_size", "radius", rescale_pal(range), name = name,
                     breaks = breaks, labels = labels, limits = limits, trans = trans,
                     guide = guide)
}
#' @export
scale_edge_size <- scale_edge_size_continuous
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales rescale_pal
#' @export
scale_edge_size_discrete <- function(..., range = c(2, 6)) {
    discrete_scale("edge_size", "size_d", function(n) {
        area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
        sqrt(area)
    }, ...)
}
#' @importFrom ggplot2 waiver continuous_scale
#' @importFrom scales abs_area rescale_max
#' @export
scale_edge_size_area <- function(..., max_size = 6) {
    continuous_scale("edge_size", "area",
                     palette = abs_area(max_size),
                     rescaler = rescale_max, ...)
}
#' @export
scale_edge_size_manual <- function(..., values) {
    manual_scale("edge_size", values, ...)
}
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_size_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_size", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
