#' @importFrom ggplot2 discrete_scale
#' @importFrom scales linetype_pal
#' @export
scale_edge_linetype <- function(..., na.value = "blank") {
    discrete_scale("edge_linetype", "linetype_d", linetype_pal(),
                   na.value = na.value, ...)
}
#' @export
scale_edge_linetype_continuous <- function(...) {
    stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
#' @export
scale_edge_linetype_discrete <- scale_edge_linetype
#' @export
scale_edge_linetype_manual <- function(..., values) {
    manual_scale("edge_linetype", values, ...)
}
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @importFrom scales identity_pal
#' @export
scale_edge_linetype_identity <- function(..., guide = "none") {
    sc <- discrete_scale("edge_linetype", "identity", identity_pal(), ..., guide = guide)

    # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
    # object should in the first place be created with the correct parent.
    sc$super <- ScaleDiscreteIdentity
    class(sc) <- class(ScaleDiscreteIdentity)
    sc
}
