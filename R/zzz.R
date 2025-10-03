.onLoad <- function(...) {
  rlang::run_on_load()
  register_s3_method("gganimate", "layer_type", "GeomEdgePath")

  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

#' @importFrom ggplot2 update_geom_defaults aes from_theme
#' @importFrom rlang on_load
#' @importFrom scales col_mix
on_load(
  if (exists("element_geom", asNamespace("ggplot2"))) {
    edge_line_aes <- aes(
      edge_colour   = from_theme(colour %||% ink),
      edge_width    = from_theme(linewidth),
      edge_linetype = from_theme(linetype)
    )
    update_geom_defaults(GeomAxisHive, aes(
      !!!edge_line_aes,
      label_size = from_theme(fontsize %||% 3.88),
      family     = from_theme(family %||% "")
    ))
    update_geom_defaults(GeomEdgePath, aes(
      !!!edge_line_aes,
      label_size    = from_theme(fontsize %||% 3.88),
      family        = from_theme(family %||% "")
    ))
    update_geom_defaults(GeomEdgeSegment, edge_line_aes)
    update_geom_defaults(GeomEdgePoint, aes(
      edge_shape  = from_theme(pointshape),
      edge_colour = from_theme(colour %||% ink),
      edge_size   = from_theme(pointsize ),
      edge_fill   = from_theme(fill %||% NA),
      stroke      = from_theme(borderwidth)
    ))
    update_geom_defaults(GeomEdgeTile, aes(
      edge_fill     = from_theme(fill %||% col_mix(ink, paper, 0.2)),
      edge_colour   = from_theme(colour %||% NA),
      edge_width    = from_theme(0.2 * borderwidth),
      edge_linetype = from_theme(bordertype)
    ))
    update_geom_defaults(GeomEdgeBezier, edge_line_aes)
    update_geom_defaults(GeomEdgeBspline, edge_line_aes)
    update_geom_defaults(GeomEdgeDensity, aes(
      edge_fill = from_theme(fill %||% col_mix(ink, paper, 0.664))
    ))
    update_geom_defaults(GeomEdgeSf, aes(
      edge_colour   = from_theme(colour %||% ink),
      edge_width    = from_theme(borderwidth),
      edge_linetype = from_theme(bordertype)
    ))
    update_geom_defaults(GeomNodeTile, aes(
      fill = from_theme(fill %||% NA),
      colour = from_theme(colour %||% ink)
    ))
  }
)
