#' Define simple shapes for line capping
#'
#' This set of functions makes it easy to define shapes at the terminal points
#' of edges that are used to shorten the edges. The shapes themselves are not
#' drawn, but the edges will end at the boundary of the shape rather than at
#' the node position. This is especially relevant when drawing arrows at the
#' edges as the arrows will be partly obscured by the node unless the edge is
#' shortened. Edge shortening is dynamic and will update as the plot is resized,
#' making sure that the capping remains at an absolute distance to the end
#' point.
#'
#' @details
#' `geometry` is the base constructor, while the rest are helpers to save
#' typing. `circle` creates circles width a given radius, `square`
#' creates squares at a given side length, `ellipsis` creates ellipses with
#' given a and b values (width and height radii), and `rectangle` makes
#' rectangles of a given width and height. label_rect is a helper that, given
#' a list of strings and potentially formatting options creates a rectangle that
#' encloses the string.
#'
#' @param type The type of geometry to use. Currently `'circle'` and
#' `'rect'` is supported.
#'
#' @param width,height,length,radius,a,b The dimensions of the shape.
#'
#' @param unit,width_unit,height_unit,a_unit,b_unit The unit for the numbers
#' given.
#'
#' @return A geometry object encoding the specified shape.
#'
#' @examples
#' geometry(c('circle', 'rect', 'rect'), 1:3, 3:1)
#'
#' circle(1:4, 'mm')
#'
#' label_rect(c('some', 'different', 'words'), fontsize = 18)
#' @export
#'
geometry <- function(type = 'circle', width = 1, height = width,
                     width_unit = 'cm', height_unit = width_unit) {
  l <- max(length(type), length(width), length(height))
  g <- rep_len(vec_cast(type, character()), l)
  g_na <- is.na(g)
  g[g_na] <- 'circle'
  width <- rep_len(vec_cast(width, double()), l)
  width[g_na] <- 0
  height <- rep_len(vec_cast(height, double()), l)
  height[g_na] <- 0
  uwidth <- rep_len(vec_cast(width_unit, character()), l)
  uheight <- rep_len(vec_cast(height_unit, character()), l)
  new_geometry(
    geometry = g,
    width = width,
    height = height,
    width_unit = uwidth,
    height_unit = uheight
  )
}
new_geometry <- function(geometry = character(), width = numeric(),
                         height = numeric(), width_unit = character(),
                         height_unit = character()) {
  vec_assert(geometry, character())
  vec_assert(width, double())
  vec_assert(height, double())
  vec_assert(width_unit, character())
  vec_assert(height_unit, character())
  new_rcrd(list(
    geometry = geometry,
    width = width,
    height = height,
    width_unit = width_unit,
    height_unit = height_unit
  ), class = 'ggraph_geometry')
}
#' @rdname geometry
#'
#' @export
circle <- function(radius = 1, unit = 'cm') {
  geometry('circle', width = radius * 2, width_unit = unit)
}
#' @rdname geometry
#'
#' @export
square <- function(length = 1, unit = 'cm') {
  geometry('rect', width = length, width_unit = unit)
}
#' @rdname geometry
#'
#' @export
ellipsis <- function(a = 1, b = 1, a_unit = 'cm', b_unit = a_unit) {
  geometry('circle',
    width = a * 2, height = b * 2, width_unit = a_unit,
    height_unit = b_unit
  )
}
#' @rdname geometry
#'
#' @export
rectangle <- function(width = 1, height = 1, width_unit = 'cm',
                      height_unit = width_unit) {
  geometry('rect',
    width = width, height = height, width_unit = width_unit,
    height_unit = height_unit
  )
}
#' @rdname geometry
#'
#' @param label The text to be enclosed
#'
#' @param padding extra size to be added around the text using the
#' [ggplot2::margin()] function
#'
#' @param ... Passed on to [grid::gpar()]
#'
#' @export
#' @importFrom grid convertWidth convertHeight textGrob grobWidth grobHeight
label_rect <- function(label, padding = margin(1, 1, 1.5, 1, 'mm'), ...) {
  grobs <- lapply(label, textGrob, gp = gpar(...))
  width <- abs_width(grobs)
  height <- abs_height(grobs)
  width <- width + sum(convertWidth(padding[c(2, 4)], 'cm', TRUE))
  height <- height + sum(convertHeight(padding[c(1, 3)], 'cm', TRUE))
  geometry('rect', width = width, height = height)
}
#' @rdname geometry
#'
#' @param x An object to test for geometry inheritance
#'
#' @export
is.geometry <- function(x) inherits(x, 'ggraph_geometry')
#' @export
format.ggraph_geometry <- function(x, ...) {
  paste0(
    field(x, 'geometry'), '(', field(x, 'width'), field(x, 'width_unit'),
    ', ', field(x, 'height'), field(x, 'height_unit'), ')'
  )
}
#' @export
is.na.ggraph_geometry <- function(x) {
  is.na(field(x, 'geometry'))
}
geo_type <- function(x) {
  if (!is.geometry(x)) cli::cli_abort('{.arg x} must be a {.cls ggraph_geometry} object')
  field(x, 'geometry')
}
geo_width <- function(x) {
  if (!is.geometry(x)) cli::cli_abort('{.arg x} must be a {.cls geometry} object')
  unit(field(x, 'width'), field(x, 'width_unit'))
}
geo_height <- function(x) {
  if (!is.geometry(x)) cli::cli_abort('{.arg x} must be a {.cls geometry} object')
  unit(field(x, 'height'), field(x, 'height_unit'))
}
#' @importFrom grid convertHeight grobHeight
abs_height <- function(grobs) {
  vapply(grobs, function(g) convertHeight(grobHeight(g), 'cm', TRUE), numeric(1))
}
#' @importFrom grid convertWidth grobWidth
abs_width <- function(grobs) {
  vapply(grobs, function(g) convertWidth(grobWidth(g), 'cm', TRUE), numeric(1))
}

#' @export
vec_ptype2.ggraph_geometry.ggraph_geometry <- function(x, y, ...) new_geometry()
#' @export
vec_ptype2.ggraph_geometry.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ggraph_geometry <- function(x, y, ...) character()
#' @export
vec_cast.character.ggraph_geometry <- function(x, to, ...) as.character(field(x, 'geometry'))

#' Define default scale type for geometry
#'
#' This function is quite useless as geometry is not meant to be scaled, but it
#' is a requirement for ggplot2 to handle it correctly.
#'
#' @export
#'
#' @keywords internal
scale_type.ggraph_geometry <- function(x) 'identity'
