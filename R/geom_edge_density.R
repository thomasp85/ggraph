#' Show edges as a density map
#'
#' This geom makes it possible to add a layer showing edge presence as a density
#' map. Each edge is converted to `n` points along the line and a jitter is
#' applied. Based on this dataset a two-dimensional kernel density estimation is
#' applied and plotted as a raster image. The density is mapped to the alpha
#' level, making it possible to map a variable to the fill.
#'
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_density` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' **x**
#' **y**
#' **xend**
#' **yend**
#' edge_fill
#' filter
#'
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for each pixel in the raster}
#'  \item{density}{The density associated with the pixel}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_raster
#'
#' @param n The number of points to estimate in the x and y direction, i.e. the
#' resolution of the raster.
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull') %>%
#'   activate(edges) %>%
#'   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#'
#' ggraph(gr, 'stress') +
#'   geom_edge_density(aes(fill = class)) +
#'   geom_edge_link() + geom_node_point()
#' @rdname geom_edge_density
#' @name geom_edge_density
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatLink
#' @importFrom MASS bandwidth.nrd kde2d
#' @export
StatEdgeDensity <- ggproto('StatEdgeDensity', Stat,
  compute_group = function(data, scales, na.rm = FALSE, h = NULL,
                             n = 100, bins = NULL, binwidth = NULL) {
    group <- data$group[1]
    x_range <- diff(range(c(data$x, data$xend)))
    y_range <- diff(range(c(data$y, data$yend)))
    x_extend <- x_range / 10
    y_extend <- y_range / 10
    data <- StatLink$compute_panel(data, n = 50)
    data <- PositionJitter$compute_layer(data, list(
      width = x_extend,
      height = y_extend
    ))

    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }
    dens <- MASS::kde2d(data$x, data$y,
      h = h, n = n,
      lims = c(
        scales$x$dimension(),
        scales$y$dimension()
      ) + c(
        -x_extend,
        x_extend,
        -y_extend,
        y_extend
      )
    )
    df <- expand.grid(x = dens$x, y = dens$y)
    df$z <- as.vector(dens$z)
    df$group <- group
    names(df) <- c('x', 'y', 'density', 'group')
    df
  },
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data
  },
  default_aes = aes(filter = TRUE),
  required_aes = c('x', 'y', 'xend', 'yend')
)
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid gTree gList grobName rasterGrob rectGrob gpar
GeomEdgeDensity <- ggproto('GeomEdgeDensity', GeomRaster,
  draw_panel = function(self, data, panel_scales, coord, ...) {
    groups <- split(data, factor(data$group))
    max_density <- max(rowSums(do.call(
      cbind,
      lapply(groups, `[[`, i = 'density')
    )))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_scales, coord,
        max.alpha = max_density,
        ...
      )
    })
    grobs <- gTree(children = do.call('gList', grobs))
    grobs$name <- grobName(grobs, 'geom_edge_density')
    grobs
  },
  draw_group = function(data, panel_scales, coord, max.alpha) {
    if (!inherits(coord, 'CoordCartesian')) {
      stop('geom_raster only works with Cartesian coordinates',
        call. = FALSE
      )
    }
    data <- coord$transform(data, panel_scales)
    x_pos <- as.integer((data$x - min(data$x)) / resolution(
      data$x,
      FALSE
    ))
    y_pos <- as.integer((data$y - min(data$y)) / resolution(
      data$y,
      FALSE
    ))
    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1
    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
    raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(
      data$edge_fill,
      data$density / max.alpha
    )
    x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
    y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))
    rasterGrob(raster,
      x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng),
      default.units = 'native', interpolate = TRUE
    )
  },
  draw_key = function(data, params, size) {
    rectGrob(gp = gpar(
      col = NA, fill = alpha(data),
      lty = 0
    ))
  },
  required_aes = c('x', 'y'),
  default_aes = aes(edge_fill = 'darkgrey')
)
#' @rdname geom_edge_density
#'
#' @importFrom ggforce StatLink
#' @export
geom_edge_density <- function(mapping = NULL, data = get_edges('short'),
                              position = 'identity', show.legend = NA,
                              n = 100, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(x = x, y = y,
                                        xend = xend, yend = yend))
  layer(
    data = data, mapping = mapping, stat = StatEdgeDensity,
    geom = GeomEdgeDensity, position = position,
    show.legend = show.legend, inherit.aes = FALSE,
    params = expand_edge_aes(
      list(na.rm = FALSE, n = n, ...)
    )
  )
}
