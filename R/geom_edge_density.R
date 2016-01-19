#' @importFrom ggplot2 ggproto Stat PositionJitter
#' @importFrom ggforce StatLink
#' @export
StatEdgeDensity <- ggproto('StatEdgeDensity', Stat,
    compute_group = function(data, scales, na.rm = FALSE, h = NULL,
                             n = 100, bins = NULL, binwidth = NULL) {
        group <- data$group[1]
        xRange <- diff(range(c(data$x, data$xend)))
        yRange <- diff(range(c(data$y, data$yend)))
        xExtend <- xRange/10
        yExtend <- yRange/10
        data <- StatLink$compute_panel(data, n = 50)
        data <- PositionJitter$compute_layer(data, list(width = xExtend, height = yExtend))

        if (is.null(h)) {
            h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
        }
        dens <- MASS::kde2d(data$x, data$y, h = h, n = n,
                            lims = c(scales$x$dimension(), scales$y$dimension()) + c(-xExtend, xExtend, -yExtend, yExtend))
        df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
        df$group <- group
        names(df) <- c("x", "y", "density", "group")
        df
    },
    required_aes = c('x', 'y', 'xend', 'yend')
)
#' @importFrom ggplot2 ggproto GeomRaster alpha
#' @importFrom grid gTree gList grobName rasterGrob rectGrob gpar
GeomEdgeDensity <- ggproto('GeomEdgeDensity', GeomRaster,
    draw_panel = function(self, data, panel_scales, coord, ...) {
        groups <- split(data, factor(data$group))
        maxDensity <- max(rowSums(do.call(cbind, lapply(groups, `[[`, i = 'density'))))
        grobs <- lapply(groups, function(group) {
            self$draw_group(group, panel_scales, coord, max.alpha = maxDensity, ...)
        })
        grobs <- gTree(children = do.call("gList", grobs))
        grobs$name <- grobName(grobs, 'geom_edge_density')
        grobs
    },
    draw_group = function(data, panel_scales, coord, max.alpha) {
        if (!inherits(coord, "CoordCartesian")) {
            stop("geom_raster only works with Cartesian coordinates",
                 call. = FALSE)
        }
        data <- coord$transform(data, panel_scales)
        x_pos <- as.integer((data$x - min(data$x))/resolution(data$x,
                                                              FALSE))
        y_pos <- as.integer((data$y - min(data$y))/resolution(data$y,
                                                              FALSE))
        nrow <- max(y_pos) + 1
        ncol <- max(x_pos) + 1
        raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
        raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(data$edge_fill,
                                                        data$density/max.alpha)
        x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
        y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))
        rasterGrob(raster, x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                   height = diff(y_rng), default.units = "native", interpolate = TRUE)
    },
    draw_key = function(data, params, size) {
        rectGrob(gp = gpar(col = NA, fill = alpha(data),
                           lty = 0))
    },
    required_aes = c('x', 'y'),
    default_aes = aes(edge_fill = 'darkgrey')
)
#' @importFrom ggplot2 layer aes
#' @importFrom ggforce StatLink
#' @export
geom_edge_density <- function(mapping = NULL, data = gEdges('short'), stat = "edge_density",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, n=100, ...) {
    mapping <- completeEdgeAes(mapping)
    mapping <- aesIntersect(mapping, aes(x=x, y=y, xend=xend, yend=yend))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeDensity,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, n = n, ...)
    )
}
