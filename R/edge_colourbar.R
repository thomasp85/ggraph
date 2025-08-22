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
#' @export
guide_edge_colourbar <- function(
  ...,
  available_aes = c("edge_colour", "edge_fill")
) {
  guide <- guide_colourbar(..., available_aes = available_aes)
  guide$params$name <- 'edge_colourbar'
  guide
}
#' @rdname guide_edge_colourbar
#' @export
guide_edge_colorbar <- guide_edge_colourbar

#' Coloursteps legend for edges
#'
#' This function is equivalent to [ggplot2::guide_coloursteps()] but
#' works for edge aesthetics.
#'
#' @inheritParams ggplot2::guide_coloursteps
#' @inheritParams ggplot2::guide_colourbar
#' @inheritDotParams ggplot2::guide_colourbar
#'
#' @return A guide object
#'
#' @export
guide_edge_coloursteps <- function(
  even.steps = TRUE,
  show.limits = NULL,
  ...,
  available_aes = c("edge_colour", "edge_fill")
) {
  guide <- guide_coloursteps(
    even.steps = even.steps,
    show.limits = show.limits,
    ...,
    available_aes = available_aes
  )
  guide$params$name <- 'edge_coloursteps'
  guide
}
#' @rdname guide_edge_coloursteps
#' @export
guide_edge_colorsteps <- guide_edge_coloursteps
