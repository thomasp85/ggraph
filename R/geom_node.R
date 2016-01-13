#' @importFrom ggplot2 GeomText
#' @export
geom_node_text <- function(mapping = NULL, data = NULL, stat = "filter",
                           position = "identity", parse = FALSE, nudge_x = 0,
                           nudge_y = 0, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
    mapping <- aesIntersect(mapping, aes(x=x, y=y, label=label, filter=TRUE))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomText,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(parse = parse, check_overlap = check_overlap,
                        na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 GeomText
#' @export
geom_node_point <- function(mapping = NULL, data = NULL, stat = "filter",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, ...) {
    mapping <- aesIntersect(mapping, aes(x=x, y=y, filter=TRUE))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
    )
}
