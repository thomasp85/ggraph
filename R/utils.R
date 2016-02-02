#' @export
nAngle <- function(x, y, degrees = TRUE) {
    angles <- atan(y/x)
    if (degrees) {
        angles*360/(2*pi)
    } else {
        angles
    }
}
#' @export
eAngle <- function(x, y, xend, yend, degrees = TRUE) {
    x <- xend - x
    y <- yend - y
    nAngle(x, y, degrees)
}


### COPY FROM GGPLOT2 NON-EXPORTS
#' @importFrom scales rescale_mid
mid_rescaler <- function(mid) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        rescale_mid(x, to, from, mid)
    }
}
manual_scale <- function(aesthetic, values, ...) {
    pal <- function(n) {
        if (n > length(values)) {
            stop("Insufficient values in manual scale. ", n,
                 " needed but only ", length(values), " provided.",
                 call. = FALSE)
        }
        values
    }
    discrete_scale(aesthetic, "manual", pal, ...)
}
#' @importFrom scales zero_range
resolution <- function(x, zero = TRUE) {
    if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
        return(1)
    x <- unique(as.numeric(x))
    if (zero) {
        x <- unique(c(0, x))
    }
    min(diff(sort(x)))
}
"%||%" <- function(a, b) {
    if (!is.null(a))
        a
    else b
}
ggname <- function(prefix, grob) {
    grob$name <- grobName(grob, prefix)
    grob
}
#' @importFrom ggplot2 calc_element element_grob
element_render <- function(theme, element, ..., name = NULL) {
    el <- calc_element(element, theme)
    if (is.null(el)) {
        message("Theme element ", element, " missing")
        return(zeroGrob())
    }
    ggname(paste(element, name, sep = "."), element_grob(el, ...))
}
