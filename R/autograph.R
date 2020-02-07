#' Quickplot wrapper for networks
#'
#' This function is intended to quickly show an overview of your network data.
#' While it returns a ggraph object that layers etc can be added to it is
#' limited in use and should not be used as a foundation for more complicated
#' plots. It allows colour, labeling and sizing of nodes and edges, and the
#' exact combination of layout and layers will depend on these as well as the
#' features of the network. The output of this function may be fine-tuned at any
#' release and should not be considered stable. If a plot should be reproducible
#' it should be created manually.
#'
#' @param graph An object coercible to a tbl_graph
#' @param ... arguments passed on to methods
#'
#' @importFrom tidygraph as_tbl_graph .register_graph_context graph_is_tree graph_is_forest
#' @importFrom rlang quo_is_null quo_text as_quosure sym
#' @export
#'
#' @examples
#' library(tidygraph)
#' gr <- create_notable('herschel') %>%
#'   mutate(class = sample(letters[1:3], n(), TRUE)) %E>%
#'   mutate(weight = runif(n()))
#'
#' # Standard graph
#' autograph(gr)
#'
#' # Adding node labels will cap edges
#' autograph(gr, node_label = class)
#'
#' # Use tidygraph calls for mapping
#' autograph(gr, node_size = centrality_pagerank())
#'
#' # Trees are plotted as dendrograms
#' iris_tree <- hclust(dist(iris[1:4], method = 'euclidean'), method = 'ward.D2')
#' autograph(iris_tree)
#'
autograph <- function(graph, ...) {
  UseMethod('autograph')
}

#' @export
#' @rdname autograph
#' @param node_colour,edge_colour Colour mapping for nodes and edges
#' @param node_size,edge_width Size/width mapping for nodes and edges
#' @param node_label,edge_label Label mapping for nodes and edges
autograph.default <- function(graph, ..., node_colour = NULL, edge_colour = NULL,
                              node_size = NULL, edge_width = NULL,
                              node_label = NULL, edge_label = NULL) {
  node_colour <- enquo(node_colour)
  edge_colour <- enquo(edge_colour)
  node_size <- enquo(node_size)
  edge_width <- enquo(edge_width)
  node_label <- enquo(node_label)
  edge_label <- enquo(edge_label)
  graph <- as_tbl_graph(graph)
  .register_graph_context(graph, TRUE)

  if (graph_is_tree() || graph_is_forest()) {
    node_names <- names(as_tibble(graph, active = 'nodes'))
    height <- if ('height' %in% node_names) quo(height) else quo(NULL)
    p <- ggraph(graph, 'dendrogram', height = !!height) +
      geom_edge_elbow0(aes(colour = !!edge_colour, width = !!edge_width))
    if (!quo_is_null(node_colour) || !quo_is_null(node_size)) {
      p <- p +
        geom_node_point(aes(filter = leaf, colour = !!node_colour, size = !!node_size))
    }
    if (!quo_is_null(node_label)) {
      p <- p +
        geom_node_text(aes(filter = leaf, label = !!node_label, colour = !!node_colour), angle = 40, hjust = 1, vjust = 1) +
        coord_cartesian(clip = 'off')
    }
  } else {
    p <- suppressMessages(ggraph(graph) + coord_fixed())

    if (!quo_is_null(node_label)) {
      label_col <- quo_text(node_label)
      label_col <- paste0(c('node1.', 'node2.'), label_col)
      start_label <- sym(label_col[1])
      end_label <- sym(label_col[2])
      p <- p +
        geom_edge_link(aes(colour = !!edge_colour, label = !!edge_label,
                           width = !!edge_width, start_cap = label_rect(!!start_label),
                           end_cap = label_rect(!!end_label)),
                       angle_calc = 'along', label_dodge = unit(2.5, 'mm'))
    } else if (!quo_is_null(edge_label)) {
      p <- p +
        geom_edge_link(aes(colour = !!edge_colour, label = !!edge_label,
                            width = !!edge_width),
                        angle_calc = 'along', label_dodge = unit(2.5, 'mm'))
    } else {
      p <- p +
        geom_edge_link0(aes(colour = !!edge_colour, width = !!edge_width))
    }
    if (quo_is_null(node_label)) {
      p <- p +
        geom_node_point(aes(colour = !!node_colour, size = !!node_size))
    } else {
      p <- p +
        geom_node_text(aes(label = !!node_label, colour = !!node_colour))
    }
  }

  p
}

#' Deprecated autograph predecessor
#'
#' @export
#' @keywords internal
qgraph <- function(...) {
  .Deprecated('autograph', msg = 'To avoid name clashing with qgraph::qgraph(), ggraph::qgraph() has been renamed to ggraph::autograph()')
}

utils::globalVariables(c(
  'leaf'
))
