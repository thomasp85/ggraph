#' A theme tuned for graph visualizations
#'
#' When plotting graphs, networks, and trees the coordinate values are often of
#' no importance and axes are thus a distraction. `ggraph` comes with a
#' build-in theme that removes redundant elements in order to put focus on the
#' data. Furthermore the default behaviour is to use a narrow font so text takes
#' up less space. Theme colour is defined by a background and foreground colour
#' where the background defines the colour of the whole graphics area and the
#' foreground defines the colour of the strip and border. By default strip and
#' border is turned off as it is an unnecessary element unless facetting is
#' used. To add a foreground colour to a plot that is already using
#' `theme_graph` the `th_foreground` helper is provided. In order to
#' use this appearance as default use the `set_graph_style` function. An
#' added benefit of this is that it also changes the default text-related values
#' in the different geoms for a completely coherent look.
#' `unset_graph_style` can be used to revert the defaults back to their
#' default settings (that is, they are not necessarily reverted back to what
#' they were prior to calling `set_graph_style`). The `th_no_axes()` helper is
#' provided to modify an existing theme so that grid and axes are removed.
#'
#' @param background The colour to use for the background. This theme sets all
#' background elements except for plot.background to `element_blank` so
#' this controls the background for all elements of the plot. Set to `NA`
#' to remove the background (thus making the plot transparent)
#'
#' @param foreground The colour of foreground elements, specifically strip and
#' border. Set to `NA` to remove.
#'
#' @param border Logical. Should border be drawn if a foreground colour is
#' provided?
#'
#' @param family,base_family,title_family,subtitle_family,strip_text_family,caption_family The font to use for the different elements
#'
#' @param base_size,size,text_size,title_size,subtitle_size,strip_text_size,caption_size The size to use for the various text elements. `text_size` will be used as geom defaults
#'
#' @param face,title_face,subtitle_face,strip_text_face,caption_face The fontface to use for the various text elements
#'
#' @param title_margin,subtitle_margin,caption_margin The margin to use between the text elements and the plot area
#'
#' @param text_colour,bg_text_colour,fg_text_colour,title_colour,subtitle_colour,strip_text_colour,caption_colour The colour of the text in the various text elements
#'
#' @param plot_margin The plot margin
#'
#' @export
#'
#' @examples
#' library(tidygraph)
#' graph <- as_tbl_graph(highschool)
#'
#' ggraph(graph) + geom_edge_link() + geom_node_point() + theme_graph()
theme_graph <- function(base_family = 'Arial Narrow', base_size = 11,
                        background = 'white', foreground = NULL, border = TRUE,
                        text_colour = 'black', bg_text_colour = text_colour,
                        fg_text_colour = text_colour,
                        title_family = base_family, title_size = 18,
                        title_face = 'bold', title_margin = 10,
                        title_colour = bg_text_colour,
                        subtitle_family = base_family, subtitle_size = 12,
                        subtitle_face = 'plain', subtitle_margin = 15,
                        subtitle_colour = bg_text_colour,
                        strip_text_family = base_family, strip_text_size = 10,
                        strip_text_face = 'bold', strip_text_colour = fg_text_colour,
                        caption_family = base_family, caption_size = 9,
                        caption_face = 'italic', caption_margin = 10,
                        caption_colour = bg_text_colour,
                        plot_margin = margin(30, 30, 30, 30)) {
  style <- theme_bw(base_size = base_size, base_family = base_family)
  style <- style + theme(
    text = element_text(colour = text_colour),
    plot.title = element_text(
      family = title_family,
      size = title_size,
      face = title_face,
      colour = title_colour,
      margin = margin(b = title_margin)
    ),
    plot.subtitle = element_text(
      family = subtitle_family,
      size = subtitle_size,
      face = subtitle_face,
      colour = subtitle_colour,
      margin = margin(b = subtitle_margin)
    ),
    plot.caption = element_text(
      family = caption_family,
      size = caption_size,
      face = caption_face,
      colour = caption_colour,
      margin = margin(t = caption_margin)
    ),
    strip.text = element_text(
      family = strip_text_family,
      size = strip_text_size,
      face = strip_text_face,
      colour = strip_text_colour
    ),

    plot.margin = plot_margin,

    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),

    strip.background = if (is.null(foreground)) element_blank() else element_rect(fill = foreground, colour = foreground),
    plot.background = if (is.null(background)) element_blank() else element_rect(fill = background, colour = NA),
    panel.border = if (border && !is.null(foreground)) element_rect(fill = NA, colour = foreground) else element_blank()
  )

  style
}
#' @rdname theme_graph
#'
#' @export
th_foreground <- function(foreground = 'grey80', fg_text_colour = NULL, border = FALSE) {
  th <- theme(
    strip.background = if (is.null(foreground)) element_blank() else element_rect(fill = foreground, colour = foreground),
    panel.border = if (border && !is.null(foreground)) element_rect(fill = NA, colour = foreground) else element_blank()
  )
  if (!is.null(fg_text_colour)) {
    th <- th + theme(strip.text = element_text(colour = fg_text_colour))
  }
  th
}
#' @rdname theme_graph
#'
#' @export
th_no_axes <- function() {
  theme(
    panel.grid = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.grid.minor.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.x.bottom = element_blank(),
    #axis.title.x.top = element_blank(),
    #axis.title.y = element_blank(),
    #axis.title.y.left = element_blank(),
    #axis.title.y.right = element_blank(),
    axis.text = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.x.top = element_blank(),
    #axis.text.x.bottom = element_blank(),
    #axis.text.y = element_blank(),
    #axis.text.y.left = element_blank(),
    #axis.text.y.right = element_blank(),
    axis.ticks = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.ticks.x.top = element_blank(),
    #axis.ticks.x.bottom = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.ticks.y.left = element_blank(),
    #axis.ticks.y.right = element_blank(),
    axis.line = element_blank(),
    #axis.line.x = element_blank(),
    #axis.line.x.bottom = element_blank(),
    #axis.line.x.top = element_blank(),
    #axis.line.y = element_blank(),
    #axis.line.y.left = element_blank(),
    #axis.line.y.right = element_blank(),
    NULL
  )
}
#' @rdname theme_graph
#'
#' @param ... Parameters passed on the `theme_graph`
#'
#' @export
set_graph_style <- function(family = 'Arial Narrow', face = 'plain', size = 11,
                            text_size = 11, text_colour = 'black', ...) {
  style <- theme_graph(
    base_family = family, base_size = size,
    text_colour = text_colour, ...
  )
  theme_set(style)
  text_size <- text_size / .pt

  update_geom_defaults(GeomEdgePath, list(
    family = family,
    fontface = face,
    label_size = text_size
  ))
  update_geom_defaults(GeomText, list(
    family = family,
    fontface = face,
    size = text_size
  ))
  update_geom_defaults(GeomTextRepel, list(
    family = family,
    fontface = face,
    size = text_size
  ))
  update_geom_defaults(GeomLabel, list(
    family = family,
    fontface = face,
    size = text_size
  ))
  update_geom_defaults(GeomLabelRepel, list(
    family = family,
    fontface = face,
    size = text_size
  ))
}
#' @rdname theme_graph
#'
#' @export
unset_graph_style <- function() {
  style <- theme_gray()
  theme_set(style)

  update_geom_defaults(GeomEdgePath, list(
    family = '',
    fontface = 1,
    label_size = 3.88
  ))
  update_geom_defaults(GeomText, list(
    family = '',
    fontface = 1,
    size = 3.88
  ))
  update_geom_defaults(GeomTextRepel, list(
    family = '',
    fontface = 1,
    size = 3.88
  ))
  update_geom_defaults(GeomLabel, list(
    family = '',
    fontface = 1,
    size = 3.88
  ))
  update_geom_defaults(GeomLabelRepel, list(
    family = '',
    fontface = 1,
    size = 3.88
  ))
  update_geom_defaults(GeomAxisHive, list(
    family = '',
    fontface = 1,
    size = 3.88
  ))
}
