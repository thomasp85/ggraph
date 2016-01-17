#' @importFrom ggplot2 discrete_scale
#' @importFrom scales shape_pal
#' @export
scale_edge_shape <- function(..., solid = TRUE) {
    discrete_scale("edge_shape", "shape_d", shape_pal(solid), ...)
}
#' @export
scale_edge_shape_discrete <- scale_edge_shape
#' @export
scale_edge_shape_continuous <- function(...) {
    stop("A continuous variable can not be mapped to shape", call. = FALSE)
}
#' @export
scale_edge_shape_manual <- function(..., values) {
    manual_scale("edge_shape", values, ...)
}
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_shape_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_shape", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
