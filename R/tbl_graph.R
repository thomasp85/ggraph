#' @rdname ggraph
#' @aliases layout_tbl_graph
#'
#' @importFrom igraph gorder
#' @export
#'
create_layout.tbl_graph <- function(graph, layout, circular = FALSE, ...) {
  graph <- mutate(activate(graph, 'nodes'), .ggraph.orig_index = seq_len(graph_order()))
  graph <- prepare_graph(graph, layout, ...)
  .register_graph_context(graph, free = TRUE)
  if (gorder(graph) == 0) {
    layout <- new_data_frame(list(x = numeric(), y = numeric(), circular = logical()))
    layout <- cbind(layout, .N())
  } else {
    layout <- layout_to_table(layout, graph, circular = circular, ...)
  }
  layout$.ggraph.index <- seq_len(nrow(layout))
  if (is.null(attr(layout, 'graph'))) {
    attr(layout, 'graph') <- graph
  }
  attr(layout, 'circular') <- circular
  class(layout) <- c(
    'layout_tbl_graph',
    'layout_ggraph',
    'data.frame'
  )
  check_layout(layout)
}
collect_edges.layout_tbl_graph <- function(layout) {
  gr <- attr(layout, 'graph')
  edges <- as_tibble(gr, active = 'edges')
  edges$circular <- rep(attr(layout, 'circular'), nrow(edges))
  as.data.frame(edges, stringsAsFactors = FALSE)
}
#' @importFrom igraph shortest_paths
#' @importFrom rlang enquo eval_tidy
collect_connections.layout_tbl_graph <- function(layout, from, to, weight = NULL, mode = 'all') {
  from <- match(from, layout$.ggraph.orig_index)
  to <- match(to, layout$.ggraph.orig_index)
  weight <- eval_tidy(enquo(weight), collect_edges(layout))
  if (is.null(weight)) {
    weight <- NA
  }
  graph <- attr(layout, 'graph')
  to_ind <- split(seq_along(to), from)
  connections <- lapply(seq_along(to_ind), function(i) {
    paths <- shortest_paths(graph, as.integer(names(to_ind)[i]), to[to_ind[[i]]], mode = mode, weights = weight)$vpath
    lapply(paths, as.numeric)
  })
  connections <- unlist(connections, recursive = FALSE)
  connections[match(seq_along(to), unlist(to_ind))]
}

# HELPERS -----------------------------------------------------------------

is.igraphlayout <- function(type) {
  if (type %in% igraphlayouts) {
    TRUE
  } else if (any(paste0(c('as_', 'in_', 'with_', 'on_'), type) %in% igraphlayouts)) {
    TRUE
  } else {
    FALSE
  }
}
as.igraphlayout <- function(type) {
  if (type %in% igraphlayouts) {
    layout <- type
  } else {
    new_type <- paste0(c('as_', 'in_', 'with_', 'on_'), type)
    type_ind <- which(new_type %in% igraphlayouts)
    if (length(type_ind) == 0) {
      stop('Cannot find igraph layout')
    }
    layout <- new_type[type_ind]
  }
  paste0('layout_', layout)
}
#' @importFrom igraph gorder permute
prepare_graph <- function(graph, layout, direction = 'out', ...) {
  if (!is.character(layout)) {
    return(graph)
  }
  is_hierarchy <- layout %in% c(
    'dendrogram',
    'treemap',
    'circlepack',
    'partition'
  )
  if (is_hierarchy || (layout == 'auto' && with_graph(graph, graph_is_tree() || graph_is_forest()))) {
    graph <- graph_to_tree(graph, mode = direction)
    graph <- permute(graph, match(seq_len(gorder(graph)), order(node_depth(graph, direction))))
  }
  as_tbl_graph(graph)
}
#' @importFrom igraph degree unfold_tree components induced_subgraph vertex_attr vertex_attr<- is.directed simplify
graph_to_tree <- function(graph, mode) {
  if (!is.directed(graph)) {
    stop('Graph must be directed')
  }
  graph <- simplify(graph, edge.attr.comb = 'first')
  parent_dir <- if (mode == 'out') 'in' else 'out'
  comp <- components(graph, 'weak')
  graphs <- lapply(seq_len(comp$no), function(i) {
    graph <- induced_subgraph(graph, which(comp$membership == i))
    n_parents <- degree(graph, mode = parent_dir)
    if (!any(n_parents == 0)) {
      stop('No root in graph. Provide graph with one parentless node')
    }
    if (any(n_parents > 1)) {
      message('Multiple parents. Unfolding graph')
      root <- which(degree(graph, mode = parent_dir) == 0)
      if (length(root) > 1) {
        message('Multiple roots in graph. Choosing the first')
        root <- root[1]
      }
      tree <- unfold_tree(graph, mode = mode, roots = root)
      vattr <- lapply(vertex_attr(graph), `[`, i = tree$vertex_index)
      vertex_attr(tree$tree) <- vattr
      graph <- tree$tree
    }
    as_tbl_graph(graph)
  })
  do.call(bind_graphs, graphs)
}
#' @importFrom igraph gorder as_edgelist delete_vertex_attr is.named
tree_to_hierarchy <- function(graph, mode, sort.by, weight, height = NULL) {
  if (is.named(graph)) graph <- delete_vertex_attr(graph, 'name')
  parent_col <- if (mode == 'out') 1 else 2
  node_col <- if (mode == 'out') 2 else 1
  edges <- as_edgelist(graph)
  hierarchy <- new_data_frame(list(parent = rep(0, gorder(graph))))
  hierarchy$parent[edges[, node_col]] <- edges[, parent_col]
  if (is.null(sort.by)) {
    hierarchy$order <- seq_len(nrow(hierarchy)) + 1
  } else {
    hierarchy$order <- order(sort.by) + 1
  }
  if (is.null(height)) {
    hierarchy$height <- 1
  } else {
    hierarchy$height <- 0
    hierarchy$height[edges[, node_col]] <- height
  }
  leaf <- degree(graph, mode = mode) == 0
  if (is.null(weight)) {
    hierarchy$weight <- 0
    hierarchy$weight[leaf] <- 1
  } else {
    if (!is.numeric(weight)) {
      stop('Weight must be numeric')
    }
    hierarchy$weight <- weight
    if (any(hierarchy$weight[!leaf] != 0)) {
      message('Non-leaf weights ignored')
    }
    if (any(hierarchy$weight[leaf] == 0)) {
      stop('Leafs must have a weight')
    }
    hierarchy$weight[!leaf] <- 0
  }
  hierarchy <- hierarchy[c(1, seq_len(nrow(hierarchy))), ]
  hierarchy$parent[1] <- -1
  hierarchy$order[1] <- 1
  hierarchy
}
#' @importFrom igraph bfs degree gorder
node_depth <- function(graph, mode) {
  mode_rev <- switch(
    mode,
    `in` = 'out',
    out = 'in',
    stop('unknown mode')
  )
  root <- which(degree(graph, mode = mode_rev) == 0)
  depth <- rep(NA_integer_, gorder(graph))
  for (i in root) {
    depth[!is.finite(depth)] <- unname(bfs(graph, root = i, unreachable = FALSE, dist = TRUE)$dist)[!is.finite(depth)]
  }
  depth
}
#' @importFrom rlang .data
add_direction <- function(graph, pos, direction = 'out') {
  if (igraph::gsize(graph) == 0) {
    return(graph)
  }
  graph <- activate(graph, 'edges')
  graph <- mutate(graph, direction = ifelse(pos$x[.data$to] < pos$x[.data$from], 'right', 'left'))
  if (direction == 'in') {
    graph <- mutate(graph, ifelse(.data$direction == 'left', 'right', 'left'))
  }
  graph <- mutate(graph, direction = factor(.data$direction))
  graph
}

#' Convert a layout to a table
#'
#' This generic takes care of dispatching various layout types (names,
#' functions, tables) to their respective functions that will return a valid
#' layout table.
#'
#' @param layout A supported object
#' @param graph A `tbl_graph`
#' @param ... passed on to implementations
#'
#' @return A valid data.frame
#'
#' @keywords internal
#' @export
layout_to_table <- function(layout, graph, ...) {
  UseMethod('layout_to_table')
}
#' @export
layout_to_table.default <- function(layout, graph, ...) {
  stop('Unknown layout', call. = FALSE)
}
#' @export
layout_to_table.character <- function(layout, graph, circular, ...) {
  if (is.igraphlayout(layout)) {
    layout_tbl_graph_igraph(graph, layout, circular, ...)
  } else {
    layout_fun <- get(paste0('layout_tbl_graph_', layout))
    layout_fun(graph, circular = circular, ...)
  }
}
#' @export
layout_to_table.matrix <- function(layout, graph, ...) {
  layout <- new_data_frame(list(x = layout[, 1], y = layout[, 2]))
  layout_to_table(layout, graph, ...)
}
#' @export
layout_to_table.data.frame <- function(layout, graph, ...) {
  cbind(layout, as_tibble(graph, active = 'nodes'))
}
#' @export
layout_to_table.function <- function(layout, graph, circular, ...) {
  layout <- if ('circular' %in% names(formals(layout))) {
    layout(graph, circular = circular, ...)
  } else {
    layout(graph, ...)
  }
  if (!is.tbl_graph(layout) && !is.data.frame(layout)) {
    layout <- tryCatch(
      as.data.frame(layout, stringsAsFactors = FALSE),
      error = function(e) {
        tryCatch(
          as_tbl_graph(layout),
          error = function(e) {
            stop('layout function must return an object coerceble to either a data.frame or tbl_graph', call. = FALSE)
          }
        )
      }
    )
  }
  if (is.tbl_graph(layout)) {
    graph <- layout
    layout <- as_tibble(graph, active = 'nodes')
    attr(layout, 'graph') <- graph
  } else {
    layout <- cbind(layout, as_tibble(graph, active = 'nodes'))
  }
  layout
}

igraphlayouts <- c(
  'as_bipartite',
  'as_star',
  'as_tree',
  'in_circle',
  'nicely',
  'with_dh',
  'with_drl',
  'with_gem',
  'with_graphopt',
  'on_grid',
  'with_mds',
  'with_sugiyama',
  'on_sphere',
  'randomly',
  'with_fr',
  'with_kk',
  'with_lgl'
)
