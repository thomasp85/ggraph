#' Get the angle of nodes and edges
#'
#' These helper functions makes it easy to calculate the angle associated with
#' nodes and edges. For nodes the angle is defined as the angle of the vector
#' pointing towards the node position, and is thus mainly suited for circular
#' layouts where it can be used to calculate the angle of labels. For edges it
#' is simply the angle of the vector describing the edge.
#'
#' @param x,y A vector of positions
#'
#' @param xend,yend The end position of the edge
#'
#' @param degrees Logical. Should the angle be returned in degree (\code{TRUE})
#' or radians (\code{FALSE}). Defaults to \code{TRUE}.
#'
#' @return A vector with the angle of each node/edge
#'
#' @examples
#' require(igraph)
#' flareGraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#'
#' ggraph(flareGraph, 'dendrogram', circular = TRUE) +
#'   geom_edge_diagonal0() +
#'   geom_node_text(aes(filter = leaf, angle = node_angle(x, y), label = shortName),
#'                  hjust = 'outward', size = 2) +
#'   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
#'
#' @export
#'
node_angle <- function(x, y, degrees = TRUE) {
    angles <- atan2(y, x)
    angles[angles < 0] <- angles[angles < 0] + 2*pi
    if (degrees) {
        angles*360/(2*pi)
    } else {
        angles
    }
}
#' @rdname node_angle
#' @usage NULL
#' @export
nAngle <- function(...) {
    .Deprecated('node_angle')
    node_angle(...)
}
#' @rdname node_angle
#'
#' @export
edge_angle <- function(x, y, xend, yend, degrees = TRUE) {
    x <- xend - x
    y <- yend - y
    node_angle(x, y, degrees)
}
#' @rdname node_angle
#' @usage NULL
#' @export
eAngle <- function(...) {
    .Deprecated('edge_angle')
    edge_angle(...)
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
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
    grob$name <- grobName(grob, prefix)
    grob
}
element_render <- function(theme, element, ..., name = NULL) {
    el <- calc_element(element, theme)
    if (is.null(el)) {
        message("Theme element ", element, " missing")
        return(zeroGrob())
    }
    ggname(paste(element, name, sep = "."), element_grob(el, ...))
}

#' @importFrom viridis scale_color_viridis
#' @export
viridis::scale_color_viridis

#' @importFrom viridis scale_fill_viridis
#' @export
viridis::scale_fill_viridis
