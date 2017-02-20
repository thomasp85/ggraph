#' Create small multiples based on edge attributes
#'
#' This function is equivalent to \code{\link[ggplot2]{facet_wrap}} but only
#' facets edges. Nodes are repeated in every panel.
#'
#' @inheritParams ggplot2::facet_wrap
#'
#' @family ggraph-facets
#'
#' @examples
#' library(igraph)
#' gr <- graph_from_data_frame(highschool)
#'
#' ggraph(gr) +
#'     geom_edge_link() +
#'     geom_node_point() +
#'     facet_edges(~year)
#'
#' @export
#'
facet_edges <- function(facets, nrow = NULL, ncol = NULL, scales = 'fixed',
                        shrink = TRUE, labeller = 'label_value', as.table = TRUE,
                        switch = NULL, drop = TRUE, dir = 'h',
                        strip.position = 'top') {
    facet <- facet_wrap(facets = facets, nrow = nrow, ncol = ncol,
                        scales = scales, shrink = shrink, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop,
                        dir = dir, strip.position = strip.position)
    ggproto(NULL, FacetEdges,
            shrink = shrink,
            params = facet$params
    )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetEdges <- ggproto('FacetEdges', FacetWrap,
    compute_layout = function(data, params) {
        plot_data <- data[[1]]
        data <- split(data, sapply(data, dataType))
        facet_data <- data$edge_ggraph

        panels <- FacetWrap$compute_layout(facet_data, params)
        node_placement <- rep(list(plot_data$ggraph.index), nrow(panels))
        names(node_placement) <- as.character(seq_along(node_placement))
        structure(panels, node_placement = node_placement)
    },
    map_data = function(data, layout, params) {
        switch(
            dataType(data),
            node_ggraph = {
                node_map <- rep(list(data), length(attr(layout, 'node_placement')))
                panel <- rep(seq_along(node_map), vapply(node_map, nrow, numeric(1)))
                node_map <- do.call(rbind, node_map)
                node_map$PANEL <- as.factor(panel)
                node_map
            },
            edge_ggraph = ,
            {
                FacetWrap$map_data(data, layout, params)
            }
        )
    }
)
