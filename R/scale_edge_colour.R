#' Edge colour scales
#'
#' This set of scales defines new colour scales for edge geoms equivalent to the
#' ones already defined by ggplot2. The parameters are equivalent to the ones
#' from ggplot2 so there is nothing new under the sun. The different geoms will
#' know whether to use edge scales or the standard scales so it is not necessary
#' to write `edge_colour` in the call to the geom - just use `colour`.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_colour
#' @rdname scale_edge_colour
#'
NULL

#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_hue
#'
#' @export
scale_edge_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "edge_colour") {
  scale_colour_hue(..., h = h, c = c, l = l, h.start = h.start,
                   direction = direction, na.value = na.value, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_brewer
#'
#' @export
scale_edge_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1,
                                     aesthetics = "edge_colour") {
  scale_colour_brewer(..., type = type, palette = palette, direction = direction,
                      aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_distiller
#'
#' @export
scale_edge_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1,
                                        values = NULL, space = "Lab", na.value = "grey50",
                                        guide = "edge_colourbar", aesthetics = "edge_colour") {
  scale_color_distiller(..., type = type, palette = palette, direction = direction,
                        values = values, space = space, na.value = na.value,
                        guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_gradient
#'
#' @export
scale_edge_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                       na.value = "grey50", guide = "edge_colourbar",
                                       aesthetics = "edge_colour") {
  scale_colour_gradient(..., low = low, high = high, space = space, na.value = na.value,
                        guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @importFrom scales muted
#' @export
scale_edge_colour_gradient2 <- function(..., low = muted("red"), mid = "white",
                                        high = muted("blue"), midpoint = 0,
                                        space = "Lab", na.value = "grey50",
                                        guide = "edge_colourbar", aesthetics = "edge_colour") {
  scale_colour_gradient2(..., low = low, mid = mid, high = high, midpoint = midpoint,
                         space = space, na.value = na.value, guide = guide,
                         aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_gradientn
#'
#' @export
scale_edge_colour_gradientn <- function(..., colours, values = NULL, space = "Lab",
                                        na.value = "grey50", guide = "edge_colourbar",
                                        aesthetics = "edge_colour", colors) {
  scale_colour_gradientn(..., colours = colours, values = values, space = space,
                         na.value = na.value, guide = guide, aesthetics = aesthetics,
                         colors = colors)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_grey
#'
#' @export
scale_edge_colour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red",
                                   aesthetics = "edge_colour") {
  scale_colour_grey(..., start = start, end = end, na.value = na.value, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_identity
#'
#' @export
scale_edge_colour_identity <- function(..., guide = "none", aesthetics = "edge_colour") {
  scale_colour_identity(..., guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_manual
#'
#' @export
scale_edge_colour_manual <- function(..., values, aesthetics = "edge_colour", breaks = waiver(), na.value = "grey50") {
  scale_colour_manual(..., values = values, aesthetics = aesthetics, breaks = breaks,
                      na.value = na.value)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams viridis::scale_colour_viridis
#'
#' @importFrom viridis scale_colour_viridis
#' @export
scale_edge_colour_viridis <- function (..., alpha = 1, begin = 0, end = 1, direction = 1,
                                       discrete = FALSE, option = "D", aesthetics = 'edge_colour') {
  scale_colour_viridis(..., alpha = alpha, begin = begin, end = end, direction = direction,
                       discrete = discrete, option = option, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_steps
#' @export
scale_edge_colour_steps <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                    na.value = "grey50", guide = "edge_coloursteps",
                                    aesthetics = "edge_colour") {
  scale_colour_steps(..., low = low, high = high, space = space, na.value = na.value,
                     guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_steps2
#' @export
scale_edge_colour_steps2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                     midpoint = 0, space = "Lab", na.value = "grey50",
                                     guide = "edge_coloursteps",
                                     aesthetics = "edge_colour") {
  scale_colour_steps2(..., low = low, mid = mid, high = high, midpoint = midpoint, space = space,
                      na.value = na.value, guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_steps2
#' @export
scale_edge_colour_stepsn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                     guide = "edge_coloursteps", aesthetics = "edge_colour", colors) {
  scale_colour_stepsn(..., colours = colours, values = values, space = space,
                      na.value = na.value, guide = guide, aesthetics = aesthetics,
                      colors = colors)
}
#' @rdname scale_edge_colour
#'
#' @inheritParams ggplot2::scale_colour_steps2
#' @export
scale_edge_colour_fermenter <- function(..., type = "seq", palette = 1, direction = -1,
                                        na.value = "grey50", guide = "edge_coloursteps",
                                        aesthetics = "edge_colour") {
  scale_colour_fermenter(..., type = type, palette = palette, direction = direction,
                      na.value = na.value, guide = guide, aesthetics = aesthetics)
}
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_colour_continuous <- scale_edge_colour_gradient
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_colour_discrete <- scale_edge_colour_hue
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_colour_binned <- scale_edge_colour_steps

#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_hue <- scale_edge_colour_hue
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_brewer <- scale_edge_colour_brewer
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_distiller <- scale_edge_colour_distiller
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_gradient <- scale_edge_colour_gradient
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_gradient2 <- scale_edge_colour_gradient2
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_gradientn <- scale_edge_colour_gradientn
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_grey <- scale_edge_colour_grey
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_identity <- scale_edge_colour_identity
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_manual <- scale_edge_colour_manual
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_continuous <- scale_edge_colour_continuous
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_discrete <- scale_edge_colour_discrete
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_viridis <- scale_edge_colour_viridis
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_steps <- scale_edge_colour_steps
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_steps2 <- scale_edge_colour_steps2
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_stepsn <- scale_edge_colour_stepsn
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_fermenter <- scale_edge_colour_fermenter
#' @rdname scale_edge_colour
#'
#' @export
scale_edge_color_binned <- scale_edge_colour_binned
