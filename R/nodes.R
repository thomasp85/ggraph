#' Create a node extractor function
#'
#' This function returns another function that can extract nodes from a
#' ggraph_layout object. As a ggraph_layout object is essentially a data.frame
#' of nodes it might seem useless to provide this function, but since the node
#' data is not necessarily available until after the \code{ggraph()} call it
#' can be beneficial to be able to add information to the node data on a
#' per-layer basis. Unlike \code{\link{gEdges}} the use of \code{gNodes} is not
#' mandatory and is only required if additional data should be added to selected
#' node layers.
#'
#' @param ... Additional data that should be cbind'ed togeether with the node
#' data.
#'
#' @return A data.frame with the node data as well of any additional data
#' supplied through \code{...}
#'
#' @family extractors
#'
#' @export
#'
gNodes <- function(...) {
    function(layout) {
        nodes <- do.call(
            cbind,
            c(list(layout),
              lapply(list(...), rep, length.out = nrow(layout)),
              list(stringsAsFactors = FALSE))
        )
        structure(nodes, type_ggraph = 'node_ggraph')
    }
}
