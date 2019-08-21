#' Annotate nodes with text
#'
#' These geoms are equivalent in functionality to [ggplot2::geom_text()] and
#' [ggplot2::geom_label()] and allows for simple annotation of nodes.
#'
#' @section Aesthetics:
#' `geom_node_text` understands the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden. Italic aesthetics are required but
#' not set by default
#'
#' - **x**
#' - **y**
#' - *label*
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - hjust
#' - lineheight
#' - size
#' - vjust
#'
#' @inheritParams ggplot2::geom_text
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. By default x and y are mapped to x and y in
#' the node data.
#'
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#' Useful for offsetting text from points, particularly on discrete scales.
#'
#' @param repel If `TRUE`, text labels will be repelled from each other
#' to avoid overlapping, using the `GeomTextRepel` geom from the
#' ggrepel package.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_node_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'stress') +
#'   geom_node_point(aes(label = class))
#'
#' ggraph(gr, 'stress') +
#'   geom_node_label(aes(label = class), repel = TRUE)
#' @importFrom ggrepel GeomTextRepel
#' @export
#'
geom_node_text <- function(mapping = NULL, data = NULL, position = 'identity',
                           parse = FALSE, nudge_x = 0, nudge_y = 0,
                           check_overlap = FALSE, show.legend = NA,
                           repel = FALSE, ...) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop('Specify either `position` or `nudge_x`/`nudge_y`',
        call. = FALSE
      )
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  params <- list(parse = parse, na.rm = FALSE, ...)
  if (repel) {
    geom <- GeomTextRepel
  } else {
    geom <- GeomText
    params$check_overlap <- check_overlap
  }

  mapping <- aes_intersect(mapping, aes(x = x, y = y))
  layer(
    data = data, mapping = mapping, stat = StatFilter, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = params
  )
}
#' @rdname geom_node_text
#'
#' @inheritParams ggplot2::geom_label
#'
#' @importFrom ggrepel GeomLabelRepel
#' @export
#'
geom_node_label <- function(mapping = NULL, data = NULL, position = 'identity',
                            parse = FALSE, nudge_x = 0, nudge_y = 0,
                            label.padding = unit(0.25, 'lines'),
                            label.r = unit(0.15, 'lines'),
                            label.size = 0.25, show.legend = NA,
                            repel = FALSE, ...) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop('Specify either `position` or `nudge_x`/`nudge_y`',
        call. = FALSE
      )
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  params <- list(
    parse = parse, label.padding = label.padding,
    label.r = label.r, label.size = label.size, na.rm = FALSE, ...
  )
  if (repel) {
    geom <- GeomLabelRepel
  } else {
    geom <- GeomLabel
  }

  mapping <- aes_intersect(mapping, aes(x = x, y = y))
  layer(
    data = data, mapping = mapping, stat = StatFilter, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = FALSE,
    params = params
  )
}
