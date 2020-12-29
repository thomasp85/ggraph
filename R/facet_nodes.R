#' Create small multiples based on node attributes
#'
#' This function is equivalent to [ggplot2::facet_wrap()] but only
#' facets nodes. Edges are drawn if their terminal nodes are both present in a
#' panel.
#'
#' @inheritParams ggplot2::facet_wrap
#'
#' @family ggraph-facets
#'
#' @examples
#' library(tidygraph)
#' gr <- as_tbl_graph(highschool) %>%
#'   mutate(popularity = as.character(cut(centrality_degree(mode = 'in'),
#'     breaks = 3,
#'     labels = c('low', 'medium', 'high')
#'   )))
#' ggraph(gr) +
#'   geom_edge_link() +
#'   geom_node_point() +
#'   facet_nodes(~popularity)
#' @export
#'
facet_nodes <- function(facets, nrow = NULL, ncol = NULL, scales = 'fixed',
                        shrink = TRUE, labeller = 'label_value', as.table = TRUE,
                        switch = NULL, drop = TRUE, dir = 'h',
                        strip.position = 'top') {
  facet <- facet_wrap(
    facets = facets, nrow = nrow, ncol = ncol,
    scales = scales, shrink = shrink, labeller = labeller,
    as.table = as.table, switch = switch, drop = drop,
    dir = dir, strip.position = strip.position
  )
  seed <- sample(.Machine$integer.max, 1L)
  facet$params$facets[] <- lapply(seq_along(facet$params$facets), function(i) {
    rlang::quo(withr::with_seed(seed, !!facet$params$facets[[i]]))
  })
  ggproto(NULL, FacetNodes,
    shrink = shrink,
    params = facet$params
  )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetNodes <- ggproto('FacetNodes', FacetWrap,
  compute_layout = function(data, params) {
    plot_data <- data[[1]]
    data <- split(data, vapply(data, data_type, character(1)))
    facet_data <- data$node_ggraph

    panels <- FacetWrap$compute_layout(facet_data, params)
    node_placement <- if (nrow(panels) == 1) {
      list(`1` = plot_data$ggraph_index)
    } else {
      node_map <- FacetWrap$map_data(plot_data, panels, params)
      node_map <- node_map[order(node_map$.ggraph.index), , drop = FALSE]
      split(node_map$.ggraph.index, node_map$PANEL)
    }
    attr(panels, 'node_placement') <- node_placement
    panels
  },
  map_data = function(data, layout, params) {
    switch(
      data_type(data),
      edge_ggraph = {
        node_placement <- attr(layout, 'node_placement')
        edge_map <- Map(function(map, nodes) {
          map[map$from %in% nodes & map$to %in% nodes, , drop = FALSE]
        }, map = rep(list(data), length(node_placement)), nodes = node_placement)
        panel <- rep(seq_along(edge_map), vapply(edge_map, nrow, numeric(1)))
        edge_map <- rbind_dfs(edge_map)
        edge_map$PANEL <- factor(panel, levels = levels(layout$PANEL))
        edge_map
      },
      node_ggraph = , {
        FacetWrap$map_data(data, layout, params)
      }
    )
  }
)
