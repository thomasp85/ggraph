#' Edge fill scales
#'
#' This set of scales defines new fill scales for edge geoms equivalent to the
#' ones already defined by ggplot2. The parameters are equivalent to the ones
#' from ggplot2 so there is nothing new under the sun. The different geoms will
#' know whether to use edge scales or the standard scales so it is not necessary
#' to write \code{edge_fill} in the call to the geom - just use \code{fill}.
#'
#' @param ... Other arguments passed on to
#' \code{\link[ggplot2]{continuous_scale}} or
#' \code{\link[ggplot2]{discrete_scale}} as appropriate, to control name,
#' limits, breaks, labels and so forth.
#'
#' @param h range of hues to use, in [0, 360]
#'
#' @param c chroma (intensity of colour), maximum value varies depending on
#' combination of hue and luminance.
#'
#' @param l luminance (lightness), in [0, 100]
#'
#' @param h.start hue to start at
#'
#' @param low The low end of the gradient
#'
#' @param high The high end of the gradient
#'
#' @param mid The midpoint colour of the gradient
#'
#' @param midpoint The midpoint (in data value) of the diverging scale. Defaults
#' to 0.
#'
#' @param colours Vector of colours to use for n-colour gradient.
#'
#' @param colors As above. Use preferred spelling
#'
#' @param start gray value at low end of palette
#'
#' @param end gray value at high end of palette
#'
#' @param type One of seq (sequential), div (diverging) or qual (qualitative)
#'
#' @param palette If a string, will use that named palette. If a number, will
#' index into the list of palettes of appropriate type
#'
#' @param space colour space in which to calculate gradient. Must be "Lab" -
#' other values are deprecated.
#'
#' @param direction For hue the direction to travel around the colour wheel,
#' 1 = clockwise, -1 = counter-clockwise. For brewer and distiller the order of
#' colors in the scale. If 1, the default, colors are as output by
#' \code{\link[RColorBrewer]{brewer.pal}}. If -1, the order of colors is
#' reversed.
#'
#' @param na.value Colour to use for missing values
#'
#' @param values For gradient and distiller if colours should not be evenly
#' positioned along the gradient this vector gives the position (between 0 and
#' 1) for each colour in the colours vector. See \code{\link[scales]{rescale}}
#' for a convience function to map an arbitrary range to between 0 and 1. For
#' manual a set of aesthetic values to map data values to. If this is a
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
#' @name scale_edge_fill
#' @rdname scale_edge_fill
#'
NULL

#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales hue_pal
#' @export
scale_edge_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50") {
    discrete_scale("edge_fill", "hue", hue_pal(h, c, l, h.start, direction),
                   na.value = na.value, ...)
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales brewer_pal
#' @export
scale_edge_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1) {
    discrete_scale("edge_fill", "brewer", brewer_pal(type, palette, direction), ...)
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales gradient_n_pal brewer_pal
#' @export
scale_edge_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar") {
    # warn about using a qualitative brewer palette to generate the gradient
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete fill palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
    }
    continuous_scale("edge_fill", "distiller",
                     gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, guide = guide, ...)
    # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales seq_gradient_pal
#' @export
scale_edge_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") {
    continuous_scale("edge_fill", "gradient", seq_gradient_pal(low, high, space),
                     na.value = na.value, guide = guide, ...)
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales div_gradient_pal muted
#' @export
scale_edge_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") {
    continuous_scale("edge_fill", "gradient2",
                     div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                     rescaler = mid_rescaler(mid = midpoint))
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 continuous_scale
#' @importFrom scales gradient_n_pal
#' @export
scale_edge_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", colors) {
    colours <- if (missing(colours)) colors else colours

    continuous_scale("edge_fill", "gradientn",
                     gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales grey_pal
#' @export
scale_edge_fill_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
    discrete_scale("edge_fill", "grey", grey_pal(start, end),
                   na.value = na.value, ...)
}
#' @rdname scale_edge_fill
#'
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_fill_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_fill", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
#' @rdname scale_edge_fill
#'
#' @export
scale_edge_fill_manual <- function(..., values) {
    manual_scale("edge_fill", values, ...)
}
#' @rdname scale_edge_fill
#'
#' @export
scale_edge_fill_continuous <- scale_edge_fill_gradient
#' @rdname scale_edge_fill
#'
#' @export
scale_edge_fill_discrete <- scale_edge_fill_hue
