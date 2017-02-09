#' @export
geometry <- function(type = 'circle', width = 1, height = width, width_unit = 'cm', height_unit = width_unit) {
    l <- max(length(type), length(width), length(height))
    g <- rep(type, length.out = l)
    g_na <- is.na(g)
    width <- rep(width, length.out = l)
    width[g_na] <- 0
    height <- rep(height, length.out = l)
    height[g_na] <- 0
    uwidth <- rep(width_unit, length.out = l)
    uheight <- rep(height_unit, length.out = l)
    g[g_na] <- 'circle'
    attributes(g) <- list(width = width, uwidth = uwidth, height = height,
                          uheight = uheight, class = 'geometry')
    g
}
#' @export
circle <- function(radius = 1, unit = 'cm') {
    geometry('circle', width = radius*2, width_unit = unit)
}
#' @export
square <- function(length = 1, unit = 'cm') {
    geometry('rect', width = length, width_unit = unit)
}
#' @export
ellipsis <- function(a = 1, b = 1, a_unit = 'cm', b_unit = a_unit) {
    geometry('circle', width = a*2, height = b*2, width_unit = a_unit, height_unit = b_unit)
}
#' @export
rectangle <- function(width = 1, height = 1, width_unit = 'cm', height_unit = width_unit) {
    geometry('rect', width = width, height = height, width_unit = width_unit, height_unit = height_unit)
}
#' @export
#' @importFrom grid convertWidth convertHeight textGrob grobWidth grobHeight
label_rect <- function(label, padding = margin(1,1,1.5,1,'mm'), ...) {
    grobs <- lapply(label, textGrob, ...)
    width <- abs_width(grobs)
    height <- abs_height(grobs)
    width <- width + sum(convertWidth(padding[c(2, 4)], 'cm', TRUE))
    height <- height + sum(convertHeight(padding[c(1, 3)], 'cm', TRUE))
    geometry('rect', width = width, height = height)
}
#' @export
is.geometry <- function(x) inherits(x, 'geometry')
#' @export
length.geometry <- function(x) length(unclass(x))
#' @export
`[.geometry` <- function(x, i, ...) {
    structure(
        unclass(x)[i],
        width = attr(x, 'width')[i],
        height = attr(x, 'height')[i],
        uwidth = attr(x, 'uwidth')[i],
        uheight = attr(x, 'uheight')[i],
        class = 'geometry'
    )
}
#' @export
`[<-.geometry` <- function(x, ..., value) {
    stopifnot(is.geometry(value))
    type <- unclass(x)
    type[...] <- unclass(value)
    width <- attr(x, 'width')
    width[...] <- attr(value, 'width')
    height <- attr(x, 'height')
    height[...] <- attr(value, 'height')
    uwidth <- attr(x, 'uwidth')
    uwidth[...] <- attr(value, 'uwidth')
    uheight <- attr(x, 'uheight')
    uheight[...] <- attr(value, 'uheight')
    structure(
        type,
        width = width,
        height = height,
        uwidth = uwidth,
        uheight = uheight,
        class = 'geometry'
    )
}
#' @export
format.geometry <- function(x, ...) {
    paste0(unclass(x), '(', attr(x, 'width'), attr(x, 'uwidth'),
           ', ', attr(x, 'height'), attr(x, 'uheight'), ')')
}
#' @export
print.geometry <- function(x) {
    print(format(x))
}
#' @export
rep.geometry <- function(x, ...) {
    i <- rep(seq_along(x), ...)
    x[i]
}
#' @export
as.data.frame.geometry <- function(x, row.names = NULL, optional = FALSE, ...) {
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) ==
                                 nrows))) {
        stop(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
                         nrows), call. = FALSE)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) {
            row.names <- character()
        } else {
            row.names <- .set_row_names(nrows)
        }
    }
    structure(list(x), row.names = row.names, class = 'data.frame')
}
#' @export
c.geometry <- function(...) {
    geometries <- list(...)
    base <- do.call(c, lapply(geometries, unclass))
    g_attr <- do.call(Map, c(list(f = c), lapply(geometries, attributes)))
    g_attr$class <- 'geometry'
    attributes(base) <- g_attr
    base
}
#' @export
is.na.geometry <- function(x) {
    is.na(unclass(x))
}
geo_type <- function(x) {
    stopifnot(is.geometry(x))
    unclass(x)
}
geo_width <- function(x) {
    stopifnot(is.geometry(x))
    unit(attr(x, 'width'), attr(x, 'uwidth'))
}
geo_height <- function(x) {
    stopifnot(is.geometry(x))
    unit(attr(x, 'height'), attr(x, 'uheight'))
}
#' @importFrom grid convertHeight grobHeight
abs_height <- function(grobs) {
    sapply(grobs, function(g) convertHeight(grobHeight(g), 'cm', TRUE))
}
#' @importFrom grid convertWidth grobWidth
abs_width <- function(grobs) {
    sapply(grobs, function(g) convertWidth(grobWidth(g), 'cm', TRUE))
}
#' @export
scale_type.geometry <- function(x) 'identity'
