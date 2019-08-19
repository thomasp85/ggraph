#' ggraph extensions to ggplot2
#'
#' This help page lists all exported ggproto classes defined by ggraph. In
#' general these should be of no concern to the user as the main interface to
#' the functionality is, as with ggplot2, the `geom_*` format. As opposed
#' to ggplot2 there really aren't any use for separate `stat_*` functions
#' as they are intimately linked to each geom and mixing and matching stats and
#' geoms would only cause a lot of trouble.
#'
#' @details
#' Many of the `geom_edge_*` geoms comes in different flavors dependent on
#' the functionality required. There will always be a base geom and some will
#' have a `geom_edge_*0` and `geom_edge_*2` version. The base geom
#' will, in the case of multiple versions, draw the edge as a sequence of small
#' segments. The different aesthetics will be repeated for each segment and a
#' counter will be added the enumerates the progression of segments, so that a
#' gradient of colour or size can be added along the edge by assigning the
#' respective aesthetic to `stat(index)`. `geom_edge_*2` will also draw
#' the edge as segments but will interpolate between the aesthetics at the end
#' points. This makes it possible for an edge to interpolate node properties of
#' its end nodes. `geom_edge_*2` is less performant than the base geom so
#' use only when interpolation is needed. `geom_edge_*0` is a
#' high-performance version that usually maps directly to a grid grob. It does
#' not allow for drawing gradients or interpolations though, so use only for
#' simple edge drawings.
#'
#' @name ggraph-extensions
#' @rdname ggraph-extensions
#' @keywords internal
#'
NULL
