#' Bundle edges using force directed edge bundling
#'
#' This geom performs force directed edge bundling to reduce visual clutter.
#' It uses a self-organizing approach to bundling in which edges are modeled as flexible springs
#' that can attract each other without the need of a hierarchy.
#'
#' @section Edge aesthetic name expansion:
#' In order to avoid excessive typing edge aesthetic names are
#' automatically expanded. Because of this it is not necessary to write
#' `edge_colour` within the `aes()` call as `colour` will
#' automatically be renamed appropriately.
#'
#' @section Aesthetics:
#' `geom_edge_bundle_force` understand the following
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
#' @rdname geom_edge_bundle_force
#' @name geom_edge_bundle_force
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeBundleForce <- ggproto("StatEdgeBundleForce", StatIdentity,
  setup_data = function(data, params) {
    if (any(names(data) == "filter")) {
      if (!is.logical(data$filter)) {
        stop("filter must be logical")
      }
      data <- data[data$filter, names(data) != "filter"]
    }
    data <- remove_loop(data)
    if (!rlang::is_installed("memoise")) {
      stop("the package 'memoise' is needed for geom_edge_bundle_force(). Please install it ")
    }
    force_bundle_mem(data, params)
  },
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(filter = TRUE),
  extra_params = c("K", "C", "P", "S", "P_rate", "I", "I_rate", "compatibility_threshold", "eps")
)

#' @rdname geom_edge_bundle_force
#'
#' @export
geom_edge_bundle_force <- function(mapping = NULL, data = get_edges(),
                                   position = "identity", arrow = NULL,
                                   lineend = "butt", show.legend = NA,
                                   K = 1, C = 6, P = 1, S = 0.04, P_rate = 2,
                                   I = 50, I_rate = 2 / 3, compatibility_threshold = 0.6,
                                   eps = 1e-8, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y,
    xend = xend, yend = yend
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeBundleForce,
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
  K <- params$K
  C <- params$C
  P <- params$P
  S <- params$S
  P_rate <- params$P_rate
  I <- params$I
  I_rate <- params$I_rate
  compatibility_threshold <- params$compatibility_threshold
  eps <- params$eps

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
    rep(1, m * segments),
    do.call("rbind", elist),
    rep(idx, m),
    rep(1:m, each = segments)
  ))
  names(data_bundle) <- c("PANEL", "x", "y", "index", "group")
  cbind(data_bundle, data[rep(1:m, each = segments), extraCols, drop = FALSE])
}

force_bundle_mem <- memoise::memoise(force_bundle)
