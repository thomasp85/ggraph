#' @export
createLayout.hclust <- function(graph, layout, circular = FALSE, ...) {
    message('Converting hclust object to dendrogram')
    createLayout(as.dendrogram(graph), layout = layout, circular = circular, ...)
}
#' @export
createLayout.network <- function(graph, layout, circular = FALSE, ...) {
    message('Converting network object to igraph')
    createLayout(network_to_igraph(graph), layout = layout, circular = circular, ...)
}

#' @importFrom igraph graph_from_edgelist
#' @importFrom utils modifyList
#' @export
network_to_igraph <- function(graph) {
    if (!is.element('network', installed.packages()[,1])) {
        stop('`network` must be installed')
    }
    graph_attr_names <- network::list.network.attributes(graph)
    graph_attr <- lapply(graph_attr_names, function(n) {
        network::get.network.attribute(graph, n)
    })
    names(graph_attr) <- graph_attr_names
    if (graph_attr$hyper) {
        stop('Hypergraphs are currently unsupported', call. = FALSE)
    }

    node_attr_names <- network::list.vertex.attributes(graph)
    node_attr <- lapply(node_attr_names, function(n) {
        network::get.vertex.attribute(graph, n)
    })
    names(node_attr) <- node_attr_names

    edge_attr_names <- network::list.edge.attributes(graph)
    edge_attr <- lapply(edge_attr_names, function(n) {
        network::get.edge.attribute(graph, n)
    })
    names(edge_attr) <- edge_attr_names

    edges <- network::as.edgelist(graph)
    class(edges) <- 'matrix'
    attributes(edges) <- attributes(edges)[c('dim', 'class')]

    new_graph <- graph_from_edgelist(edges, graph_attr$directed)
    graph_attr(new_graph) <- modifyList(
        graph_attr,
        list(bipartite = NULL, directed = NULL, hyper = NULL, loops = NULL,
             mnext = NULL, multiple = NULL, n = NULL)
    )
    if (is.character(node_attr$vertex.names)) {
        node_attr$name <- node_attr$vertex.names
    }
    node_attr$vertex.names <- NULL
    vertex_attr(new_graph) <- node_attr
    edge_attr(new_graph) <- edge_attr

    new_graph
}
