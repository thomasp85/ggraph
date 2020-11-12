#' Create a fabric layout
#'
#' This layout is a bit unusual in that it shows nodes as horizontal line ranges
#' end edges as evenly spaced vertical spans connecting the nodes. As with the
#' matrix layout the strength comes from better scalability but its use require
#' some experience recognising the patterns that different connectivity features
#' gives rise to. As with matrix layouts the ordering of nodes have huge power
#' over the look of the plot. The `node_rank_fabric()` mimics the default
#' ordering from the original BioFabric implementation, but other ranking
#' algorithms from tidygraph can be used with the `sort.by` argument as well.
#' Fabric layouts tend to become quite wide as the graph grows which is
#' something that should be handled with care - e.g. by only zooming in on a
#' specific region.
#'
#' @param graph An `tbl_graph` object
#'
#' @param circular Ignored
#'
#' @param sort.by An expression providing the sorting of the nodes. If `NULL`
#' the nodes will be ordered by their index in the graph.
#'
#' @param shadow.edges Should shadow edges be shown.
#'
#' @return A data.frame with the columns `x`, `xmin`, `xmax`, `y`, `circular` as
#' well as any information stored as node variables in the tbl_graph object.
#' Further, the edges of the graph will gain a `edge_x` variable giving the
#' horizontal position of the edge as well as a `shadow_edge` variable denoting
#' whether the edge is a shadow edge added by the layout.
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph incident_edges V
#' @importFrom rlang enquo eval_tidy
#'
#' @references
#' BioFabric website: <http://www.biofabric.org>
#'
#' Longabaugh, William J.R. (2012).
#' *Combing the hairball with BioFabric: a new approach for visualization of large networks*.
#' BMC Bioinformatics, 13: 275. <https://doi.org/10.1186/1471-2105-13-275>
#'
layout_tbl_graph_fabric <- function(graph, circular = FALSE, sort.by = NULL, shadow.edges = FALSE) {
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  if (!is.null(sort.by)) {
    pos <- order(order(sort.by))
  } else {
    pos <- seq_len(gorder(graph))
  }

  edges <- as_edgelist(graph, names = FALSE)
  edges <- cbind(pos[edges[,1]], pos[edges[,2]])
  first_node <- pmin(edges[,1], edges[,2])
  second_node <- pmax(edges[,1], edges[,2])
  edge_order <- order(first_node, second_node)

  if (shadow.edges) {
    shadow_order <- order(second_node, first_node)
    edge_order <- split(edge_order, factor(first_node[edge_order], seq_along(pos)))
    shadow_order <- split(shadow_order + length(second_node), factor(second_node[shadow_order], seq_along(pos)))
    edge_order <- unlist(c(shadow_order, edge_order)[matrix(seq_len(length(pos) * 2), nrow = 2, byrow = T)])
    graph <- bind_edges(graph, as_tibble(graph, active = 'edges'))
    shadow <- rep(c(FALSE, TRUE), each = length(first_node))
  } else {
    shadow <- rep_len(FALSE, length(first_node))
  }

  edge_rank <- match(seq_along(edge_order), edge_order)

  node_span <- vapply(incident_edges(graph, V(graph), mode = 'all'), function(e) {
    range(edge_rank[as.integer(e)])
  }, numeric(2))

  nodes <- new_data_frame(list(x = colMeans(node_span), xmin = node_span[1,],
                      xmax = node_span[2,], y = abs(pos - max(pos))))
  extra_data <- as_tibble(graph, active = 'nodes')
  warn_dropped_vars(nodes, extra_data)
  nodes <- cbind(nodes, extra_data[, !names(extra_data) %in% names(nodes), drop = FALSE])
  nodes$circular <- FALSE

  graph <- activate(graph, 'edges')
  graph <- mutate(graph, edge_x = edge_rank, shadow_edge = shadow)
  graph <- activate(graph, 'nodes')
  attr(nodes, 'graph') <- graph
  nodes
}

#' @rdname layout_tbl_graph_fabric
#' @importFrom igraph bfs degree
#' @importFrom tidygraph activate .G arrange
#' @export
node_rank_fabric <- function() {
  graph <- activate(.G(), 'nodes')
  graph <- mutate(graph, node_order_orig = seq_len(n()))
  graph <- arrange(graph, -degree(graph))
  node_order_orig <- pull(graph, node_order_orig)
  graph <- activate(graph, 'edges')
  graph <- arrange(graph, pmin(from, to), pmax(from, to))
  order <- as.integer(bfs(graph, 1, 'all', order = TRUE)$order)
  order <- node_order_orig[order]
  match(seq_along(order), order)
}
