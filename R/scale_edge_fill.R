#' Edge fill scales
#'
#' This set of scales defines new fill scales for edge geoms equivalent to the
#' ones already defined by ggplot2. The parameters are equivalent to the ones
#' from ggplot2 so there is nothing new under the sun. The different geoms will
#' know whether to use edge scales or the standard scales so it is not necessary
#' to write `edge_fill` in the call to the geom - just use `fill`.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_edge_fill
#' @rdname scale_edge_fill
#'
NULL

#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_hue
#'
#' @importFrom scales hue_pal
#' @export
scale_edge_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = 'grey50') {
  discrete_scale('edge_fill', 'hue', hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_brewer
#'
#' @importFrom scales brewer_pal
#' @export
scale_edge_fill_brewer <- function(..., type = 'seq', palette = 1, direction = 1) {
  discrete_scale('edge_fill', 'brewer', brewer_pal(type, palette, direction), ...)
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_distiller
#'
#' @importFrom scales gradient_n_pal brewer_pal
#' @export
scale_edge_fill_distiller <- function(..., type = 'seq', palette = 1, direction = -1, values = NULL, space = 'Lab', na.value = 'grey50', guide = 'edge_colourbar') {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c('seq', 'div', 'qual'))
  if (type == 'qual') {
    warning('Using a discrete fill palette in a continuous scale.\n  Consider using type = "seq" or type = "div" instead', call. = FALSE)
  }
  continuous_scale('edge_fill', 'distiller',
    gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space),
    na.value = na.value, guide = guide, ...
  )
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_gradient
#' @param low,high Colours for low and high ends of the gradient.
#'
#' @importFrom scales seq_gradient_pal
#' @export
scale_edge_fill_gradient <- function(..., low = '#132B43', high = '#56B1F7', space = 'Lab', na.value = 'grey50', guide = 'edge_colourbar') {
  continuous_scale('edge_fill', 'gradient', seq_gradient_pal(low, high, space),
    na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @export
scale_edge_fill_gradient2 <- function(..., low = muted('red'), mid = 'white', high = muted('blue'), midpoint = 0, space = 'Lab', na.value = 'grey50', guide = 'edge_colourbar') {
  continuous_scale('edge_fill', 'gradient2',
    div_gradient_pal(low, mid, high, space),
    na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint)
  )
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#'
#' @importFrom scales gradient_n_pal
#' @export
scale_edge_fill_gradientn <- function(..., colours, values = NULL, space = 'Lab', na.value = 'grey50', guide = 'edge_colourbar', colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale('edge_fill', 'gradientn',
    gradient_n_pal(colours, values, space),
    na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_grey
#'
#' @importFrom scales grey_pal
#' @export
scale_edge_fill_grey <- function(..., start = 0.2, end = 0.8, na.value = 'red') {
  discrete_scale('edge_fill', 'grey', grey_pal(start, end),
    na.value = na.value, ...
  )
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_edge_fill_identity <- function(..., guide = 'none') {
  sc <- discrete_scale('edge_fill', 'identity', identity_pal(), ...,
    guide = guide, super = ScaleDiscreteIdentity
  )
  sc
}
#' @rdname scale_edge_fill
#'
#' @inheritParams ggplot2::scale_fill_manual
#'
#' @export
scale_edge_fill_manual <- function(..., values) {
  manual_scale('edge_fill', values, ...)
}
#' @rdname scale_edge_fill
#'
#' @inheritParams viridis::scale_fill_viridis
#'
#' @importFrom viridis viridis viridis_pal
#' @export
scale_edge_fill_viridis <- function(..., alpha = 1, begin = 0, end = 1,
                                    discrete = FALSE, option = 'D',
                                    direction = 1) {
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }
  if (discrete) {
    discrete_scale(
      'edge_fill', 'viridis',
      viridis_pal(
        alpha = alpha, begin = begin, end = end,
        option = option
      ), ...
    )
  } else {
    scale_edge_fill_gradientn(colours = viridis(256,
      alpha = alpha,
      begin = begin, end = end,
      option = option
    ), ...)
  }
}
#' @rdname scale_edge_fill
#'
#' @export
scale_edge_fill_continuous <- scale_edge_fill_gradient
#' @rdname scale_edge_fill
#'
#' @export
scale_edge_fill_discrete <- scale_edge_fill_hue
