# ---------------------------------------------------------------------------
# Translations for R/geom_axis_hive.R
# ---------------------------------------------------------------------------

# TODO: waiting for an example

# ---------------------------------------------------------------------------
# Translations for R/geom_edge.R
# ---------------------------------------------------------------------------

toPath <- function(data, prestats_data, layout, params, p, ...) {
  data$alpha <- data$alpha %||% params$edge_alpha %||% 1
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomEdgePath <- toPath

#' @export
to_basic.GeomEdgeSegment <- toPath

#' @export
to_basic.GeomEdgeBezier <- toPath

#' @export
to_basic.GeomEdgeBspline <- toPath

# ---------------------------------------------------------------------------
# Translations for R/geom_edge_density.R
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# Translations for R/geom_treemap.R
# ---------------------------------------------------------------------------

#' @export
to_basic.GeomTreemap <- getFromNamespace("to_basic.GeomRect", asNamespace("plotly"))


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}

