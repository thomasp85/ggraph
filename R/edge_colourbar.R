#' Colourbar legend for edges
#'
#' This function is equivalent to [ggplot2::guide_colourbar()] but
#' works for edge aesthetics.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @inheritDotParams ggplot2::guide_colourbar
#'
#' @return A guide object
#'
#' @importFrom grid is.unit unit
#' @importFrom digest digest
#' @export
guide_edge_colourbar <- function(..., available_aes = c("edge_colour", "edge_fill")) {
  guide <- guide_colourbar(..., available_aes = available_aes)
  guide$name <- 'edge_colourbar'
  guide
}
#' @rdname guide_edge_colourbar
#' @export
guide_edge_colorbar <- guide_edge_colourbar
