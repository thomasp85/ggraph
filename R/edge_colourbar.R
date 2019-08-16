#' Colourbar legend for edges
#'
#' This function is equivalent to [ggplot2::guide_colourbar()] but
#' works for edge aesthetics.
#'
#' @inheritParams ggplot2::guide_colourbar
#'
#' @return A guide object
#'
#' @importFrom grid is.unit unit
#' @importFrom digest digest
#' @export
guide_edge_colourbar <- function(title = waiver(), title.position = NULL,
                                 title.theme = NULL, title.hjust = NULL,
                                 title.vjust = NULL, label = TRUE,
                                 label.position = NULL, label.theme = NULL,
                                 label.hjust = NULL, label.vjust = NULL,
                                 barwidth = NULL, barheight = NULL, nbin = 20,
                                 raster = TRUE, ticks = TRUE, draw.ulim = TRUE,
                                 draw.llim = TRUE, direction = NULL,
                                 default.unit = 'line', reverse = FALSE, order = 0,
                                 ...) {
  if (!is.null(barwidth) && !is.unit(barwidth)) {
    barwidth <- unit(barwidth, default.unit)
  }
  if (!is.null(barheight) && !is.unit(barheight)) {
    barheight <- unit(barheight, default.unit)
  }
  guide <- list(
    title = title, title.position = title.position,
    title.theme = title.theme, title.hjust = title.hjust,
    title.vjust = title.vjust, label = label,
    label.position = label.position, label.theme = label.theme,
    label.hjust = label.hjust, label.vjust = label.vjust,
    barwidth = barwidth, barheight = barheight, nbin = nbin,
    raster = raster, ticks = ticks, draw.ulim = draw.ulim,
    draw.llim = draw.llim, direction = direction,
    default.unit = default.unit, reverse = reverse, order = order,
    available_aes = c('edge_colour', 'edge_fill'), ...,
    name = 'edge_colourbar'
  )
  class(guide) <- c('guide', 'edge_colourbar', 'colorbar')
  guide
}
#' @rdname guide_edge_colourbar
#' @export
guide_edge_colorbar <- guide_edge_colourbar

#' @importFrom scales discard
#' @importFrom stats setNames
#' @rdname guide-helpers
#' @export
guide_train.edge_colourbar <- function(guide, scale, aesthetic = NULL) {
  if (length(intersect(scale$aesthetics, c(
    'edge_colour',
    'edge_fill'
  ))) == 0) {
    warning('edge_colourbar guide needs edge_colour or edge_fill scales.')
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning('edge_colourbar guide needs continuous scales.')
    return(NULL)
  }
  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks))) {
    return()
  }
  ticks <- as.data.frame(setNames(
    list(scale$map(breaks)),
    aesthetic %||% scale$aesthetics[1]
  ), stringsAsFactors = FALSE)
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks)
  guide$key <- ticks
  .limits <- scale$get_limits()
  .bar <- discard(pretty(.limits, n = guide$nbin), scale$get_limits())
  if (length(.bar) == 0) {
    .bar <- unique(.limits)
  }
  guide$bar <- new_data_frame(list(
    colour = scale$map(.bar), value = .bar,
    stringsAsFactors = FALSE
  ))
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, digest::digest(list(
    title, key$.label,
    bar, name
  )))
  guide
}
