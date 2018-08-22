# ---------------------------------------------------------------------------
# Translations for R/geom_axis_hive.R
# ---------------------------------------------------------------------------

# TODO: waiting for an example

# ---------------------------------------------------------------------------
# Translations for custom Edge geoms
# ---------------------------------------------------------------------------

toPath <- function(data, prestats_data, layout, params, p, ...) {
  data$alpha <- data$alpha %||% params$edge_alpha %||% 1
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomEdgeBezier <- toPath

#' @export
to_basic.GeomEdgeBspline <- toPath

#' @export
to_basic.GeomEdgeDensity <- function(data, prestats_data, layout, params, p, ...) {
  # avoid a weird precision issue
  data$density[data$density < 0.005] <- 0
  data$fill_plotlyDomain <- data$density
  data$fill <- toRGB(
    data$edge_fill, scales::rescale(data$density)
  )
  prefix_class(data, "GeomTile")
}

#' @export
to_basic.GeomEdgePath <- toPath

#' @export
to_basic.GeomEdgeSegment <- toPath

#' @export
to_basic.GeomEdgePoint <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPoint")
}

# ---------------------------------------------------------------------------
# Translations for custom Node geoms
# ---------------------------------------------------------------------------

#' @export
to_basic.GeomNodeTile <- getFromNamespace("to_basic.GeomTile", asNamespace("plotly"))


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}

