#' Calculate nodes as areas dividing their parent
#'
#' The partition layout is a way to show hierarchical data in the same way as
#' [layout_tbl_graph_treemap()]. Instead of subdividing the parent area
#' the partition layout shows the division of a nodes children next to the area
#' of the node itself. As such the node positions will be very reminiscent of
#' a reingold-tilford tree layout but by plotting nodes as areas it better
#' communicate the total weight of a node by summing up all its children.
#' Often partition layouts are called icicle plots or sunburst diagrams (in case
#' a radial transform is applied).
#'
#' @note
#' partition is a layout intended for trees, that is, graphs where nodes
#' only have one parent and zero or more children. If the provided graph does
#' not fit this format an attempt to convert it to such a format will be made.
#'
#' @param graph An `tbl_graph` object
#'
#' @param weight An optional node variable to use as weight. Will only affect
#' the weight of leaf nodes as the weight of non-leaf nodes are derived from
#' their children.
#'
#' @param circular Logical. Should the layout be transformed to a circular
#' representation. If `TRUE` the resulting layout will be a sunburst
#' diagram.
#'
#' @param height An optional node variable to use as height. If `NULL`
#' all nodes will be given a height of 1.
#'
#' @param sort.by The name of a node variable to sort the nodes by.
#'
#' @param direction The direction of the tree in the graph. `'out'` (default)
#' means that parents point towards their children, while `'in'` means that
#' children point towards their parent.
#'
#' @param const.area Logical. Should 'height' be scaled for area proportionality
#' when using `circular = TRUE`. Defaults to `TRUE`.
#'
#' @param offset If `circular = TRUE`, where should it begin. Defaults to
#' `pi/2` which is equivalent to 12 o'clock.
#'
#' @return If `circular = FALSE` A data.frame with the columns `x`,
#' `y`, `width`, `height`, `leaf`,
#' `depth`, `circular` as well as any information stored as node
#' variables in the tbl_graph object.
#' If `circular = TRUE` A data.frame with the columns `x`, `y`,
#' `r0`, `r`, `start`, `end`, `leaf`,
#' `depth`, `circular` as well as any information stored as node
#' variables in the tbl_graph object.
#'
#' @references
#' Kruskal, J. B., Landwehr, J. M. (1983). *Icicle Plots: Better Displays
#' for Hierarchical Clustering*. American Statistician Vol 37(2), 162-168.
#' https://doi.org/10.2307/2685881
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom ggforce radial_trans
#'
layout_tbl_graph_partition <- function(graph, weight = NULL, circular = FALSE, height = NULL, sort.by = NULL, direction = 'out', offset = pi / 2, const.area = TRUE) {
  weight <- enquo(weight)
  weight <- eval_tidy(weight, .N())
  height <- enquo(height)
  height <- eval_tidy(height, .N())
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  hierarchy <- tree_to_hierarchy(graph, direction, sort.by, weight, height)
  layout <- partitionTree(hierarchy$parent, hierarchy$order, hierarchy$weight, hierarchy$height)[-1, ]
  if (circular) {
    if (const.area) {
      y0 <- sqrt(layout[, 2])
      y1 <- sqrt(layout[, 2] + layout[, 4])
      layout[, 2] <- y0
      layout[, 4] <- y1 - y0
    }
    width_range <- c(0, max(rowSums(layout[, c(1, 3)])))
    radial <- radial_trans(
      r.range = c(0, 1),
      a.range = width_range,
      offset = offset,
      pad = 0
    )
    coords <- radial$transform(
      layout[, 2] + layout[, 4] / 2,
      layout[, 1] + layout[, 3] / 2
    )
    layout <- new_data_frame(list(
      x = coords$x,
      y = coords$y,
      r0 = layout[, 2],
      r = layout[, 2] + layout[, 4],
      start = 2 * pi * layout[, 1] / width_range[2],
      end = 2 * pi * (layout[, 1] + layout[, 3]) / width_range[2],
      circular = TRUE
    ))
    layout$x[1] <- 0
    layout$y[1] <- 0
  } else {
    layout <- new_data_frame(list(
      x = layout[, 1] + layout[, 3] / 2,
      y = layout[, 2] + layout[, 4] / 2,
      width = layout[, 3],
      height = layout[, 4],
      circular = FALSE
    ))
  }
  layout$leaf <- degree(graph, mode = direction) == 0
  layout$depth <- node_depth(graph, mode = direction)
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(layout, extra_data)
  layout <- cbind(layout, extra_data[, !names(extra_data) %in% names(layout), drop = FALSE])
  attr(layout, 'graph') <- add_direction(graph, layout)
  layout
}
