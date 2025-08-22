#' Edge direction guide
#'
#' This guide is intended to show the direction of edges based on the aesthetics
#' mapped to its progression, such as changing width, colour and opacity.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @inheritParams ggplot2::guide_legend
#'
#' @param arrow Logical. Should an arrow be drawn to illustrate the direction.
#' Defaults to `TRUE`. The arrow is styled with the `legend.axis.line` theme
#' element. If `FALSE` the direction will be indicated by the text
#' given in `labels`
#'
#' @param labels A vector with two strings giving the labels to place at the
#' start and the end of the legend to indicate direction if `arrow = FALSE`
#'
#' @param arrow.position `r lifecycle::badge('deprecated')` The position of the
#' arrow relative to the example edge. Use the `legend.text.position` argument
#' in `theme()` instead.
#'
#' @export
#'
#' @examples
#' gr <- tidygraph::as_tbl_graph(highschool)
#' ggraph(gr, layout = 'kk') +
#'   geom_edge_fan(aes(alpha = after_stat(index))) +
#'   guides(edge_alpha = guide_edge_direction())
#'
#' # Use text labels instead of an arrow
#' ggraph(gr, layout = 'kk') +
#'   geom_edge_fan(aes(alpha = after_stat(index))) +
#'   guides(edge_alpha = guide_edge_direction(labels = c('start', 'end')))
#'
#' # Style the indicator arrow
#' arrow_style <- element_line(linewidth = 3, arrow = grid::arrow(type = "closed"))
#' ggraph(gr, layout = 'kk') +
#'   geom_edge_fan(aes(alpha = after_stat(index))) +
#'   guides(
#'     edge_alpha = guide_edge_direction(
#'       theme = theme(legend.axis.line = arrow_style)
#'     )
#'   )
#'
guide_edge_direction <- function(
  title = NULL,
  theme = NULL,
  arrow = NULL,
  labels = NULL,
  nbin = 500,
  position = NULL,
  direction = NULL,
  reverse = FALSE,
  order = 0,
  override.aes = list(),
  ...,
  available_aes = c("edge_colour", "edge_alpha", "edge_width"),
  arrow.position = deprecated()
) {
  # Piggyback on non-exported `deprecated_guide_args()`
  theme <- guide_colourbar(theme = theme, ...)$params$theme

  if (!is.null(arrow) && !is.null(labels)) {
    cli::cli_abort(
      "{.arg arrow} and {.arg labels} can't be used at the same time"
    )
  }
  if (is.null(arrow) && is.null(labels)) {
    arrow <- TRUE
  }
  if (!is.null(labels)) {
    arrow <- FALSE
  }
  if (!arrow && is.null(labels)) {
    labels <- c("from", "to")
  }

  if (lifecycle::is_present(arrow.position)) {
    lifecycle::deprecate_warn(
      '2.2.0',
      'guide_edge_direction(arrow.position)',
      'guide_edge_direction(theme = "theme(legend.text.position)")'
    )
    if (!is.null(arrow.position)) {
      if (is.null(theme)) {
        theme <- theme()
      }
      theme$legend.text.position <- arrow.position
    }
  }
  if (!is.null(position)) {
    position <- arg_match0(
      position,
      c("top", "right", "bottom", "left", "inside")
    )
  }
  if (!is.null(labels) && (length(labels) != 2 || !is.character(labels))) {
    cli::cli_abort('{.arg label} must be a vector of two strings')
  }
  new_guide(
    title = title,
    theme = theme,
    arrow = arrow,
    labels = labels,
    nbin = nbin,
    position = position,
    direction = direction,
    override.aes = expand_edge_aes(rename_aes(override.aes)),
    reverse = reverse,
    order = order,
    available_aes = available_aes,
    name = 'direction',
    super = GuideEdgeDirection
  )
}
GuideEdgeDirection <- ggproto(
  "GuideEdgeDirection",
  GuideLegend,

  params = list(
    # title
    title = NULL,

    # theming
    theme = NULL,

    # bar
    nbin = 300,
    arrow = TRUE,
    labels = c("from", "to"),

    # general
    direction = NULL,
    override.aes = list(),
    reverse = FALSE,
    order = 0,

    # parameter
    name = "direction",
    hash = character(),
    position = NULL
  ),

  available_aes = c('edge_colour', 'edge_alpha', 'edge_width'),

  hashables = exprs(title, decor$value, name),

  elements = list(
    background = "legend.background",
    margin = "legend.margin",
    key = "legend.key",
    key_height = "legend.key.height",
    key_width = "legend.key.width",
    text = "legend.text",
    theme.title = "legend.title",
    text_position = "legend.text.position",
    title_position = "legend.title.position",
    arrow_line = "legend.axis.line"
  ),

  extract_key = function(scale, aesthetic, ...) {
    if (scale$is_discrete()) {
      cli::cli_warn("{.fn guide_edge_direction} needs continuous scales.")
      return(NULL)
    }
    Guide$extract_key(scale, aesthetic, ...)
  },

  extract_decor = function(scale, aesthetic, nbin = 300, reverse = FALSE, ...) {
    limits <- scale$get_limits()
    bar <- seq(limits[1], limits[2], length.out = nbin)
    if (length(bar) == 0) {
      bar <- unique0(limits)
    }
    aes <- scale$aesthetics[1]
    bar <- data_frame0(
      {{ aes }} := scale$map(bar),
      value = bar,
      .size = length(bar)
    )
    if (reverse) {
      bar <- bar[rev(seq_len(nrow(bar))), , drop = FALSE]
    }
    return(bar)
  },

  merge = function(self, params, new_guide, new_params) {
    new_params$decor$value <- NULL
    params$decor <- vec_cbind(params$decor, new_params$decor)
    return(list(guide = self, params = params))
  },

  get_layer_key = function(params, ...) {
    decor <- GuideLegend$get_layer_key(params, ...)$decor
    extra_data <- lapply(decor, `[[`, 'data')
    missing_aes <- setdiff(
      c("edge_colour", "edge_alpha", "edge_width"),
      names(params$decor)
    )
    for (d in extra_data) {
      for (aes in missing_aes) {
        if (!is.null(d[[aes]])) params$decor[[aes]] <- d[[aes]][1]
      }
    }
    params
  },

  setup_params = function(params) {
    params$direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"),
      arg_nm = "direction"
    )
    params
  },

  setup_elements = function(params, elements, theme) {
    elements <- GuideColourbar$setup_elements(params, elements, theme)
    if (is.logical(elements$arrow_line$arrow)) {
      elements$arrow_line$arrow <- grid::arrow()
    }
    elements$arrow_line$arrow$ends <- if (params$reverse) 1L else 2L
    elements
  },

  build_labels = function(key, elements, params) {
    if (params$arrow) {
      list(
        labels = flip_element_grob(
          elements$arrow_line,
          x = unit(c(0, 1), "npc"),
          y = unit(c(0.5, 0.5), "npc"),
          flip = params$direction == "vertical"
        )
      )
    } else {
      list(
        labels = flip_element_grob(
          elements$text,
          label = params$labels,
          x = unit(c(0, 1), "npc"),
          margin_x = FALSE,
          margin_y = TRUE,
          flip = params$direction == "vertical"
        )
      )
    }
  },

  build_ticks = function(key, elements, params, position = params$position) {
    zeroGrob()
  },

  build_decor = function(decor, grobs, elements, params) {
    y <- seq(0, 1, length.out = nrow(decor) + 1)
    yend <- y[-1]
    y <- y[-length(y)]
    x <- rep(0.5, length(y))
    xend <- x
    if (params$direction == "horizontal") {
      tmp <- x
      x <- y
      y <- tmp
      tmp <- xend
      xend <- yend
      yend <- tmp
    }
    grob <- segmentsGrob(
      x0 = x,
      y0 = y,
      x1 = xend,
      y1 = yend,
      default.units = "npc",
      gp = gpar(
        col = alpha(decor$edge_colour, decor$edge_alpha),
        lwd = decor$edge_width * .pt,
        lty = decor$edge_linetyoe,
        lineend = "butt"
      )
    )

    list(bar = grob)
  },

  measure_grobs = function(grobs, params, elements) {
    params$sizes <- list(
      widths = elements$width_cm,
      heights = elements$height_cm
    )
    sizes <- GuideLegend$measure_grobs(grobs, params, elements)
    if (params$arrow) {
      l <- convertHeight(
        elements$arrow_line$arrow$length,
        "cm",
        valueOnly = TRUE
      )
      a <- 2 * pi * elements$arrow_line$arrow$angle / 360
      width <- sin(a) * l * 2 + elements$arrow_line$linewidth / 10
      if (params$direction == "vertical") {
        sizes$widths[sizes$widths == 0] <- width
      } else {
        sizes$heights[sizes$heights == 0] <- width
      }
    }
    sizes
  }
)
