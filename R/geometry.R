#' @export
geometry <- function(type = 'circle', width = 1, height = width, width_unit = 'cm', height_unit = width_unit) {
    l <- max(length(type), length(width), length(height))
    g <- structure(list(
        rep(type, length.out = l),
        rep(width, length.out = l),
        rep(height, length.out = l),
        rep(width_unit, length.out = l),
        rep(height_unit, length.out = l)
    ), class = 'geometry')
    g_na <- is.na(g)
    g[[1]][g_na] <- 'circle'
    g[[2]][g_na] <- 0
    g[[3]][g_na] <- 0
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
label_rect <- function(label, padding = margin(), ...) {
    grobs <- lapply(label, textGrob, ...)
    width <- abs_width(grobs)
    height <- abs_height(grobs)
    width <- width + sum(convertWidth(padding[c(2, 4)], 'cm', TRUE))
    height <- height + sum(convertHeight(padding[c(1, 3)], 'cm', TRUE))
    geometry('rect', width = width, height = height)
}
#' @export
length.geometry <- function(x) length(x[[1]])
#' @export
`[.geometry` <- function(x, i, ...) {
    structure(lapply(x, function(par) {
        par[i]
    }), class = 'geometry')
}
#' @export
format.geometry <- function(x, ...) {
    paste0(x[[1]], '(', x[[2]], x[[4]], ', ', x[[3]], x[[5]], ')')
}
#' @export
print.geometry <- function(x) {
    print(format(x))
}
#' @export
rep.geometry <- function(x, ...) {
    structure(lapply(x, function(par) {
        rep(par, ...)
    }), class = 'geometry')
}
#' @export
as.data.frame.geometry <- function(x, row.names = NULL, optional = FALSE, ...) {
    structure(
        list(x),
        row.names = if (is.null(row.names)) as.character(seq_along(x)) else row.names,
        class = 'data.frame'
    )
}
#' @export
c.geometry <- function(...) {
    structure(
        do.call(Map, c(list(f = c), lapply(list(...), unclass))),
        class = 'geometry'
    )
}
#' @export
is.na.geometry <- function(x) {
    is.na(x[[1]])
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
