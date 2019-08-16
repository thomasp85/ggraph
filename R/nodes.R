#' Create a node extractor function
#'
#' This function returns another function that can extract nodes from a
#' ggraph_layout object. As a ggraph_layout object is essentially a data.frame
#' of nodes it might seem useless to provide this function, but since the node
#' data is not necessarily available until after the `ggraph()` call it
#' can be beneficial to be able to add information to the node data on a
#' per-layer basis. Unlike [get_edges()] the use of `get_nodes` is not
#' mandatory and is only required if additional data should be added to selected
#' node layers.
#'
#' @param ... Additional data that should be cbind'ed together with the node
#' data.
#'
#' @return A data.frame with the node data as well of any additional data
#' supplied through `...`
#'
#' @family extractors
#'
#' @export
#'
get_nodes <- function(...) {
  function(layout) {
    nodes <- do.call(
      cbind,
      c(
        list(layout),
        lapply(list(...), rep, length.out = nrow(layout)),
        list(stringsAsFactors = FALSE)
      )
    )
    attr(nodes, 'type_ggraph') <- 'node_ggraph'
    nodes
  }
}
