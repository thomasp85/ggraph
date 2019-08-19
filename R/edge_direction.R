#' Edge direction guide
#'
#' This guide is intended to show the direction of edges based on the aesthetics
#' mapped to its progression, such as changing width, colour and opacity.
#'
#' @inheritParams ggplot2::guide_colourbar
#'
#' @param arrow Logical. Should an arrow be drawn to illustrate the direction.
#' Defaults to `TRUE`
#'
#' @param arrow.position The position of the arrow relative to the example edge.
#'
#' @param override.aes A list specifying aesthetic parameters of legend key.
#'
#' @importFrom grid is.unit unit
#' @importFrom digest digest
#' @export
#'
#' @examples
#' gr <- tidygraph::as_tbl_graph(highschool)
#' ggraph(gr, layout = 'kk') +
#'   geom_edge_fan(aes(alpha = stat(index))) +
#'   guides(edge_alpha = guide_edge_direction())
guide_edge_direction <- function(title = waiver(), title.position = NULL,
                                 title.theme = NULL, title.hjust = NULL,
                                 title.vjust = NULL, arrow = TRUE,
                                 arrow.position = NULL,
                                 barwidth = NULL, barheight = NULL, nbin = 500,
                                 direction = NULL, default.unit = 'line',
                                 reverse = FALSE, order = 0,
                                 override.aes = list(), ...) {
  if (!is.null(barwidth) && !is.unit(barwidth)) {
    barwidth <- unit(barwidth, default.unit)
  }
  if (!is.null(barheight) && !is.unit(barheight)) {
    barheight <- unit(barheight, default.unit)
  }
  guide <- list(
    title = title, title.position = title.position,
    title.theme = title.theme, title.hjust = title.hjust,
    title.vjust = title.vjust, arrow = arrow,
    arrow.position = arrow.position, barwidth = barwidth,
    barheight = barheight, nbin = nbin, direction = direction,
    default.unit = default.unit, reverse = reverse, order = order,
    available_aes = c('edge_colour', 'edge_alpha', 'edge_width'),
    override.aes = expand_edge_aes(rename_aes(override.aes)), ...,
    name = 'edge_direction'
  )
  class(guide) <- c('guide', 'edge_direction')
  guide

}
#' Helper methods for guides
#'
#' @importFrom scales discard
#' @export
#' @rdname guide-helpers
#' @keywords internal
guide_train.edge_direction <- function(guide, scale, aesthetic = NULL) {
  if (length(intersect(scale$aesthetics, c(
    'edge_colour',
    'edge_alpha',
    'edge_width'
  ))) == 0) {
    warning('edge_colourbar guide needs edge_colour or edge_alpha or edge_width scales.')
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
  guide$bar <- as.data.frame(setNames(
    list(scale$map(.bar)),
    scale$aesthetics[1]
  ), stringsAsFactors = FALSE)
  guide$bar$.value <- .bar
  guide$bar <- guide$bar[order(.bar), ]
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, digest::digest(list(
    title, bar$.value,
    direction, name
  )))
  guide
}
#' @rdname guide-helpers
#' @export
guide_merge.edge_direction <- function(guide, new_guide) {
  guide$bar <- merge(guide$bar, new_guide$bar, sort = FALSE)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) warning('Duplicated override.aes is ignored.')
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
  guide
}
#' @export
#' @rdname guide-helpers
guide_geom.edge_direction <- function(guide, layers, default_mapping) {
  guide$geoms <- lapply(layers, function(layer) {
    all <- names(c(
      layer$mapping, if (layer$inherit.aes) default_mapping,
      layer$stat$default_aes
    ))
    geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
    matched <- intersect(intersect(all, geom), names(guide$bar))
    matched <- setdiff(matched, names(layer$geom_params))
    matched <- setdiff(matched, names(layer$aes_params))
    if (length(matched) > 0) {
      if (is.na(layer$show.legend) || layer$show.legend) {
        data <- layer$geom$use_defaults(
          guide$bar[matched],
          layer$aes_params
        )
      }
      else {
        return(NULL)
      }
    }
    else {
      if (is.na(layer$show.legend) || !layer$show.legend) {
        return(NULL)
      }
      else {
        data <- layer$geom$use_defaults(
          NULL,
          layer$aes_params
        )[rep(1, nrow(guide$bar)), ]
      }
    }
    data <- utils::modifyList(data, guide$override.aes)
    list(data = data, params = c(layer$geom_params, layer$stat_params))
  })
  guide$geoms <- guide$geoms[!vapply(guide$geoms, is.null, logical(1))]
  if (length(guide$geoms) == 0) {
    guide <- NULL
  }
  guide
}
#' @importFrom grid convertWidth convertHeight unit grobWidth grobHeight arrow grobName
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom utils tail
#' @importFrom stats setNames
#' @rdname guide-helpers
#' @export
guide_gengrob.edge_direction <- function(guide, theme) {
  switch(guide$direction, horizontal = {
    arrow.position <- guide$arrow.position %||% 'bottom'
    if (!arrow.position %in% c('top', 'bottom')) {
      stop('label position "', arrow.position, '" is invalid')
    }
  }, vertical = {
    arrow.position <- guide$arrow.position %||% 'right'
    if (!arrow.position %in% c('left', 'right')) {
      stop('label position "', arrow.position, '" is invalid')
    }
  })
  arrowlength <- convertWidth(guide$arrowlength %||%
    (theme$legend.key.width * 5), 'mm')
  arrowwidth <- convertWidth(unit(sin(30 / 360 * 2 * pi) * 0.25 * 2, 'in'), 'mm')
  edgewidth <- max(vapply(guide$geoms, function(g) {
    max(g$data$edge_width)
  }, numeric(1))) * .pt
  edgewidth <- convertWidth(unit(edgewidth, 'points'), 'mm')
  hgap <- c(convertWidth(unit(0.3, 'lines'), 'mm'))
  vgap <- hgap
  x <- rep(c(edgewidth) / 2, nrow(guide$bar))
  xend <- x
  y <- seq(0, 1, length.out = nrow(guide$bar) + 1) * c(arrowlength)
  yend <- y[-1]
  y <- y[-length(y)]
  grob.bar <- switch(
    guide$direction,
    horizontal = {
      lapply(guide$geoms, function(g) {
        segmentsGrob(
          x0 = y, y0 = x, x1 = yend, y1 = xend,
          default.units = 'mm',
          gp = gpar(
            col = alpha(
              g$data$edge_colour,
              g$data$edge_alpha
            ),
            lwd = g$data$edge_width * .pt,
            lty = g$data$edge_linetype,
            lineend = 'butt'
          )
        )
      })
    },
    vertical = {
      lapply(guide$geoms, function(g) {
        segmentsGrob(
          x0 = x, y0 = y, x1 = xend, y1 = yend,
          default.units = 'mm',
          gp = gpar(
            col = alpha(
              g$data$edge_colour,
              g$data$edge_alpha
            ),
            lwd = g$data$edge_width * .pt,
            lty = g$data$edge_linetype,
            lineend = 'butt'
          )
        )
      })
    }
  )
  grob.bar <- do.call(gList, grob.bar)
  grob.title <- ggname(
    'guide.title',
    element_grob(guide$title.theme %||%
      calc_element('legend.title', theme),
    label = guide$title,
    hjust = guide$title.hjust %||%
      theme$legend.title.align %||% 0,
    vjust = guide$title.vjust %||% 0.5
    )
  )
  title_width <- convertWidth(grobWidth(grob.title), 'mm')
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), 'mm')
  title_height.c <- c(title_height)
  grob.arrow <- {
    if (!guide$arrow) {
      zeroGrob()
    } else {
      switch(
        guide$direction,
        horizontal = {
          segmentsGrob(
            x0 = y[1], y0 = c(arrowwidth) / 2, x1 = tail(yend, 1),
            y1 = c(arrowwidth) / 2, default.units = 'mm',
            gp = gpar(
              col = 'black', lwd = 0.5 * .pt,
              lty = 'solid', lineend = 'round'
            ),
            arrow = arrow(ends = if (guide$reverse) {
              'first'
            } else {
              'last'
            })
          )
        },
        vertical = {
          segmentsGrob(
            x0 = c(arrowwidth) / 2, y0 = y[1], x1 = c(arrowwidth) / 2,
            y1 = tail(yend, 1), default.units = 'mm',
            gp = gpar(
              col = 'black', lwd = 0.5 * .pt,
              lty = 'solid', lineend = 'round'
            ),
            arrow = arrow(ends = if (guide$reverse) {
              'first'
            } else {
              'last'
            })
          )
        }
      )
    }
  }
  switch(guide$direction, horizontal = {
    switch(arrow.position, top = {
      bl_widths <- c(arrowlength)
      bl_heights <- c(c(arrowwidth), vgap, c(edgewidth))
      vps <- list(
        bar.row = 3, bar.col = 1, label.row = 1,
        label.col = 1
      )
    }, bottom = {
      bl_widths <- c(arrowlength)
      bl_heights <- c(c(edgewidth), vgap, c(arrowwidth))
      vps <- list(
        bar.row = 1, bar.col = 1, label.row = 3,
        label.col = 1
      )
    })
  }, vertical = {
    switch(arrow.position, left = {
      bl_widths <- c(c(arrowwidth), vgap, c(edgewidth))
      bl_heights <- c(arrowlength)
      vps <- list(
        bar.row = 1, bar.col = 3, label.row = 1,
        label.col = 1
      )
    }, right = {
      bl_widths <- c(c(edgewidth), vgap, c(arrowwidth))
      bl_heights <- c(arrowlength)
      vps <- list(
        bar.row = 1, bar.col = 1, label.row = 1,
        label.col = 3
      )
    })
  })
  switch(guide$title.position, top = {
    widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
    heights <- c(title_height.c, vgap, bl_heights)
    vps <- with(vps, list(
      bar.row = bar.row + 2, bar.col = bar.col,
      label.row = label.row + 2, label.col = label.col,
      title.row = 1, title.col = 1:length(widths)
    ))
  }, bottom = {
    widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
    heights <- c(bl_heights, vgap, title_height.c)
    vps <- with(vps, list(
      bar.row = bar.row, bar.col = bar.col,
      label.row = label.row, label.col = label.col,
      title.row = length(heights),
      title.col = 1:length(widths)
    ))
  }, left = {
    widths <- c(title_width.c, hgap, bl_widths)
    heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
    vps <- with(vps, list(bar.row = bar.row, bar.col = bar.col +
      2, label.row = label.row, label.col = label.col +
      2, title.row = 1:length(heights), title.col = 1))
  }, right = {
    widths <- c(bl_widths, hgap, title_width.c)
    heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
    vps <- with(vps, list(
      bar.row = bar.row, bar.col = bar.col,
      label.row = label.row, label.col = label.col,
      title.row = 1:length(heights),
      title.col = length(widths)
    ))
  })
  grob.background <- element_render(theme, 'legend.background')
  padding <- unit(1.5, 'mm')
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)
  gt <- gtable(widths = unit(widths, 'mm'), heights = unit(
    heights,
    'mm'
  ))
  gt <- gtable_add_grob(gt, grob.background,
    name = 'background',
    clip = 'off', t = 1, r = -1, b = -1, l = 1
  )
  gt <- gtable_add_grob(gt, grob.bar,
    name = 'bar', clip = 'off',
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col), b = 1 +
      max(vps$bar.row), l = 1 + min(vps$bar.col)
  )
  gt <- gtable_add_grob(gt, grob.arrow,
    name = 'label', clip = 'off',
    t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
    b = 1 + max(vps$label.row), l = 1 + min(vps$label.col)
  )
  gt <- gtable_add_grob(gt, grob.title,
    name = 'title', clip = 'off',
    t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
    b = 1 + max(vps$title.row), l = 1 + min(vps$title.col)
  )
  gt
}
