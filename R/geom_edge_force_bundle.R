#' Bundle edges using force directed edge bundling
#'
#' This geom performs force directed edge bundling to reduce visual clutter.
#' It uses a self-organizing approach to bundling in which edges are modeled as flexible springs
#' that can attract each other without the need of a hierarchy.
#'
#' @section Edge variants:
#' Many geom_edge_* layers comes in 3 flavors depending on the level of control
#' needed over the drawing. The default (no numeric postfix) generate a number
#' of points (`n`) along the edge and draws it as a path. Each point along
#' the line has a numeric value associated with it giving the position along the
#' path, and it is therefore possible to show the direction of the edge by
#' mapping to this e.g. `colour = ..index..`. The version postfixed with a
#' "2" uses the "long" edge format (see [get_edges()]) and makes it
#' possible to interpolate node parameter between the start and end node along
#' the edge. It is considerable less performant so should only be used if this
#' is needed. The version postfixed with a "0" draws the edge in the most
#' performant way, often directly using an appropriate grob from the grid
#' package, but does not allow for gradients along the edge.
#'
#' Often it is beneficial to stop the drawing of the edge before it reaches the
#' node, for instance in cases where an arrow should be drawn and the arrowhead
#' shouldn't lay on top or below the node point. geom_edge_* and geom_edge_*2
#' supports this through the start_cap and end_cap aesthetics that takes a
#' [geometry()] specification and dynamically caps the termini of the
#' edges based on the given specifications. This means that if
#' `end_cap = circle(1, 'cm')` the edges will end at a distance of 1cm even
#' during resizing of the plot window.
#'
#' All `geom_edge_*` and `geom_edge_*2` have the ability to draw a
#' label along the edge. The reason this is not a separate geom is that in order
#' for the label to know the location of the edge it needs to know the edge type
#' etc. Labels are drawn by providing a label aesthetic. The label_pos can be
#' used to specify where along the edge it should be drawn by supplying a number
#' between 0 and 1. The label_size aesthetic can be used to control the size of
#' the label. Often it is needed to have the label written along the direction
#' of the edge, but since the actual angle is dependent on the plot dimensions
#' this cannot be calculated beforehand. Using the angle_calc argument allows
#' you to specify whether to use the supplied angle aesthetic or whether to draw
#' the label along or across the edge.
#'
#' @section Edge aesthetic name expansion:
#' In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' `edge_colour` within the `aes()` call as `colour` will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' `geom_edge_force_bundle` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#'
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param K spring force
#' @param C number of iteration cycles
#' @param P initial number of edge divisions
#' @param S initial step size
#' @param P_rate factor for how many new division points to add after a cycle
#' @param I number of iteration steps per cycle
#' @param I_rate factor of how to decrease the number of iterations per cycle
#' @param compatibility_threshold threshold for considering two edges to be interacting
#' @param eps tolerance
#'
#'
#' @family geom_edge_*
#'
#' @references
#' Holten, Danny, and Jarke J. Van Wijk. "Force‐Directed Edge Bundling for Graph Visualization."
#' Computer Graphics Forum (Blackwell Publishing Ltd) 28, no. 3 (2009): 983-990.
#'
#' @rdname geom_edge_force_bundle
#' @name geom_edge_force_bundle
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeForceBundle <- ggproto("StatEdgeForceBundle", StatIdentity,
  setup_data = function(data, params) {
    if (any(names(data) == "filter")) {
      if (!is.logical(data$filter)) {
        stop("filter must be logical")
      }
      data <- data[data$filter, names(data) != "filter"]
    }
    data <- remove_loop(data)
    force_bundle(data, params)
  },
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(filter = TRUE),
  extra_params = c("K", "C", "P", "S", "P_rate", "I", "I_rate", "compatibility_threshold", "eps")
)

#' @rdname geom_edge_force_bundle
#'
#' @export
geom_edge_force_bundle <- function(mapping = NULL, data = get_edges(),
                                   position = "identity", arrow = NULL,
                                   lineend = "butt", show.legend = NA,
                                   K = 1, C = 6, P = 1, S = 0.04, P_rate = 2,
                                   I = 50, I_rate = 2 / 3, compatibility_threshold = 0.1,
                                   eps = 1e-8, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y,
    xend = xend, yend = yend
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeForceBundle,
    geom = GeomEdgePath, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        K = K, C = C, P = P, S = S, P_rate = P_rate, I = I, I_rate = I_rate,
        compatibility_threshold = compatibility_threshold, eps = eps,
        interpolate = FALSE, ...
      )
    )
  )
}

force_bundle <- function(data, params) {
  # parameter handling
  K <- params$K # 1
  C <- params$C # 6
  P <- params$P # 1
  S <- params$S # 0.04
  P_rate <- params$P_rate # 2
  I <- params$I # 50
  I_rate <- params$I_rate # 2/3
  compatibility_threshold <- params$compatibility_threshold # 0.1
  eps <- params$eps # 1e-8

  # initialize matrix with coordinates
  extraCols <- !names(data) %in% c("x", "y", "xend", "yend", "group", "PANEL")
  edges_xy <- cbind(data$x, data$y, data$xend, data$yend)
  m <- nrow(edges_xy)

  # initialize edge subdivision list
  elist <- unname(lapply(
    split(edges_xy, rep(seq_len(nrow(edges_xy)), ncol(edges_xy))),
    function(y) matrix(y, 2, 2, byrow = TRUE)
  ))

  # main force bundling routine
  elist <- force_bundle_iter(
    edges_xy, elist, K, C, P, P_rate,
    S, I, I_rate, compatibility_threshold, eps
  )

  # assemble data frame
  segments <- nrow(elist[[1]])

  idx <- seq(0, 1, length.out = segments)
  data_bundle <- as.data.frame(cbind(
    rep(1, m * segments), # hard coded, pretty sure that's wrong
    do.call("rbind", elist),
    rep(idx, m),
    rep(1:m, each = segments)
  ))
  names(data_bundle) <- c("PANEL", "x", "y", "index", "group")
  cbind(data_bundle, data[rep(1:m, each = segments), extraCols, drop = FALSE])
}
