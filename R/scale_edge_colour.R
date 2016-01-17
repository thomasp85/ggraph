#' @importFrom ggplot2 discrete_scale
#' @importFrom scales hue_pal
#' @export
scale_edge_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50") {
    discrete_scale("edge_colour", "hue", hue_pal(h, c, l, h.start, direction),
                   na.value = na.value, ...)
}
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales brewer_pal
#' @export
scale_edge_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1) {
    discrete_scale("edge_colour", "brewer", brewer_pal(type, palette, direction), ...)
}
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales gradient_n_pal brewer_pal
#' @export
scale_edge_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar") {
    # warn about using a qualitative brewer palette to generate the gradient
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
    }
    continuous_scale("edge_colour", "distiller",
                     gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, guide = guide, ...)
    # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales seq_gradient_pal
#' @export
scale_edge_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") {
    continuous_scale("edge_colour", "gradient", seq_gradient_pal(low, high, space),
                     na.value = na.value, guide = guide, ...)
}
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales div_gradient_pal muted
#' @export
scale_edge_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") {
    continuous_scale("edge_colour", "gradient2",
                     div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                     rescaler = mid_rescaler(mid = midpoint))
}
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales gradient_n_pal
#' @export
scale_edge_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", colors) {
    colours <- if (missing(colours)) colors else colours

    continuous_scale("edge_colour", "gradientn",
                     gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales grey_pal
#' @export
scale_edge_colour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
    discrete_scale("edge_colour", "grey", grey_pal(start, end),
                   na.value = na.value, ...)
}
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_colour_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_colour", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
#' @export
scale_edge_colour_manual <- function(..., values) {
    manual_scale("edge_colour", values, ...)
}
#' @export
scale_edge_colour_continuous <- scale_edge_colour_gradient
#' @export
scale_edge_colour_discrete <- scale_edge_colour_hue

#' @export
scale_edge_color_hue <- scale_edge_colour_hue
#' @export
scale_edge_color_brewer <- scale_edge_colour_brewer
#' @export
scale_edge_color_distiller <- scale_edge_colour_distiller
#' @export
scale_edge_color_gradient <- scale_edge_colour_gradient
#' @export
scale_edge_color_gradient2 <- scale_edge_colour_gradient2
#' @export
scale_edge_color_gradientn <- scale_edge_colour_gradientn
#' @export
scale_edge_color_grey <- scale_edge_colour_grey
#' @export
scale_edge_color_identity <- scale_edge_colour_identity
#' @export
scale_edge_color_manual <- scale_edge_colour_manual
#' @export
scale_edge_color_continuous <- scale_edge_colour_continuous
#' @export
scale_edge_color_discrete <- scale_edge_colour_discrete
