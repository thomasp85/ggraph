#' Place nodes in a Hive Plot layout
#'
#' Hive plots were invented by Martin Krzywinski as a perceptually uniform and
#' scalable alternative to standard node-edge layouts. In hive plots nodes are
#' positioned on axes radiating out from a center based on their own information
#' e.g. membership of a class, size of neighborhood, etc. Edges are then drawn
#' between nodes as bezier curves. As the placement of nodes is not governed by
#' convoluted algorithms but directly reflects the qualities of the nodes itself
#' the resulting plot can be easier to interpret as well as compare to other
#' graphs.
#'
#' @details
#' In order to be able to draw all edges without edges crossing axes you should
#' not assign nodes to axes based on a variable with more than three levels.
#'
#' @param graph An `tbl_graph` object
#'
#' @param axis The node attribute to use for assigning nodes to axes
#'
#' @param axis.pos The relative distance to the prior axis. Default
#' (`NULL`) places axes equidistant.
#'
#' @param sort.by The node attribute to use for placing nodes along their axis.
#' Defaults (`NULL`) places nodes sequentially.
#'
#' @param divide.by An optional node attribute to subdivide each axis by.
#'
#' @param divide.order The order the axis subdivisions should appear in
#'
#' @param normalize Logical. Should axis lengths be equal or reflect the number
#' of nodes in each axis. Defaults to `TRUE`.
#'
#' @param center.size The size of the blank center, that is, the start position
#' of the axes.
#'
#' @param divide.size The distance between subdivided axis segments.
#'
#' @param use.numeric Logical, If the `sort.by` attribute is numeric,
#' should these values be used directly in positioning the nodes along the axes.
#' Defaults to `FALSE` which sorts the numeric values and positions them
#' equidistant from each other.
#'
#' @param offset Change the overall rotation of the hive plot by changing the
#' offset of the first axis.
#'
#' @param split.axes Should axes be split to show edges between nodes on the
#' same axis? One of:
#' \describe{
#'   \item{`'none'`}{Do not split axes and show in-between edges}
#'   \item{`'loops'`}{Only split axes that contain in-between edges}
#'   \item{`'all'`}{Split all axes}
#' }
#'
#' @param split.angle The angular distance between the two axes resulting from a
#' split.
#'
#' @param circular Ignored.
#'
#' @return A data.frame with the columns `x`, `y`, `r`,
#' `center_size`, `split`, `axis`, `section`, `angle`,
#' `circular` as well as any information stored as node variables in the
#' tbl_graph object.
#'
#' @references
#' Krzywinski, M., Birol, I., Jones, SJM., and Marra, MA. (2012). *Hive
#' plots-rational approach to visualizing networks*. Brief Bioinform 13 (5):
#' 627-644. https://doi.org/10.1093/bib/bbr069
#'
#' <http://www.hiveplot.net>
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder vertex_attr gsize induced_subgraph add_vertices E ends add_edges delete_edges %--% edge_attr
#' @importFrom utils tail
layout_tbl_graph_hive <- function(graph, axis, axis.pos = NULL, sort.by = NULL, divide.by = NULL, divide.order = NULL, normalize = TRUE, center.size = 0.1, divide.size = 0.05, use.numeric = FALSE, offset = pi / 2, split.axes = 'none', split.angle = pi / 6, circular = FALSE) {
  axis <- enquo(axis)
  axis <- eval_tidy(axis, .N())
  sort.by <- enquo(sort.by)
  sort.by <- eval_tidy(sort.by, .N())
  divide.by <- enquo(divide.by)
  divide.by <- eval_tidy(divide.by, .N())

  axes <- split(seq_len(gorder(graph)), axis)
  if (is.null(axis.pos)) {
    axis.pos <- rep(1, length(axes))
  } else {
    if (length(axis.pos) != length(axes)) {
      warning('Number of axes not matching axis.pos argument. Recycling as needed')
      axis.pos <- rep(axis.pos, length.out = length(axes))
    }
  }
  axis.pos <- -cumsum(axis.pos)
  axis.pos <- c(0, axis.pos[-length(axis.pos)]) / -tail(axis.pos, 1) * 2 * pi + offset
  if (use.numeric) {
    if (is.null(sort.by) || !is.numeric(sort.by)) {
      stop('sort.by must be a numeric vertex attribute when use.numeric = TRUE')
    }
    numeric.range <- range(sort.by)
  }
  if (normalize) {
    normalize_to <- rep(1, length(axes))
  } else {
    normalize_to <- lengths(axes) / max(lengths(axes))
  }
  node.pos <- Map(function(nodes, axis_length, axis, angle) {
    if (length(nodes) == 0) {
      return(new_data_frame())
    }
    split_axis <- switch(
      split.axes,
      all = TRUE,
      loops = gsize(induced_subgraph(graph, nodes)) > 0,
      none = FALSE,
      stop('Unknown split argument. Use "all", "loops" or "none"')
    )
    node_div <- axis_length / length(nodes)
    if (is.null(divide.by)) {
      node_split <- list(`1` = nodes)
    } else {
      if (use.numeric) {
        stop('Cannot divide axis while use.numeric = TRUE')
      }
      node_split <- split(nodes, divide.by[nodes])
      if (!is.null(divide.order)) {
        if (!all(divide.order %in% names(node_split))) {
          stop('All ', divide.by, ' levels must be present in divide.order')
        }
        node_split <- node_split[order(match(names(node_split), divide.order))]
      }
    }
    node_pos <- lapply(node_split, function(nodes) {
      if (length(nodes) == 0) {
        return(numeric())
      }
      if (is.null(sort.by)) {
        pos <- match(seq_along(nodes), order(nodes)) - 1
        pos <- pos * node_div
      } else {
        pos <- sort.by[nodes]
        if (use.numeric) {
          if (!is.numeric(pos)) {
            stop('sort.by must contain numeric data when use.numeric = TRUE')
          }
          if (normalize) {
            if (diff(range(pos)) == 0) {
              pos <- rep(0.5, length.out = length(pos))
            } else {
              pos <- (pos - min(pos)) / diff(range(pos))
            }
          } else {
            pos <- (pos - numeric.range[1]) / diff(numeric.range)
          }
        } else {
          pos <- match(seq_along(pos), order(pos)) - 1
          pos <- pos * node_div
        }
      }
      pos
    })
    node_pos <- Reduce(function(l, r) {
      append(l, list(r + node_div + divide.size + max(l[[length(l)]])))
    }, x = node_pos[-1], init = node_pos[1])
    node_pos <- unlist(node_pos) + center.size

    new_data_frame(list(
      node = nodes,
      r = node_pos[match(nodes, unlist(node_split))],
      center_size = center.size,
      split = split_axis,
      axis = axis,
      section = rep(names(node_split), lengths(node_split))[match(nodes, unlist(node_split))],
      angle = angle,
      circular = FALSE
    ))
  }, nodes = axes, axis_length = normalize_to, axis = names(axes), angle = axis.pos)
  for (i in seq_along(node.pos)) {
    if (nrow(node.pos[[i]]) > 0 && node.pos[[i]]$split[1]) {
      n_new_nodes <- nrow(node.pos[[i]])
      new_node_start <- gorder(graph) + 1
      extra_nodes <- node.pos[[i]]
      extra_nodes$node <- seq(new_node_start, length.out = n_new_nodes)
      vattr <- lapply(vertex_attr(graph), `[`, i = node.pos[[i]]$node)
      graph <- add_vertices(graph, n_new_nodes, attr = vattr)

      loop_edges <- E(graph)[node.pos[[i]]$node %--% node.pos[[i]]$node]
      if (length(loop_edges) != 0) {
        loop_edges_ends <- ends(graph, loop_edges, names = FALSE)
        correct_order_ends <- node.pos[[i]]$r[match(loop_edges_ends[, 1], node.pos[[i]]$node)] <
          node.pos[[i]]$r[match(loop_edges_ends[, 2], node.pos[[i]]$node)]
        loop_edges_ends <- new_data_frame(list(
          from = ifelse(correct_order_ends, loop_edges_ends[, 1], loop_edges_ends[, 2]),
          to = ifelse(correct_order_ends, loop_edges_ends[, 2], loop_edges_ends[, 1])
        ))
        loop_edges_ends$to <- extra_nodes$node[match(loop_edges_ends$to, node.pos[[i]]$node)]
        loop_edges_ends <- matrix(c(
          ifelse(correct_order_ends, loop_edges_ends$from, loop_edges_ends$to),
          ifelse(correct_order_ends, loop_edges_ends$to, loop_edges_ends$from)
        ), nrow = 2, byrow = TRUE)
        eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(loop_edges))
        graph <- add_edges(graph, as.vector(loop_edges_ends), attr = eattr)
        graph <- delete_edges(graph, as.numeric(loop_edges))
      }

      node_correction <- unlist(lapply(node.pos[-i], function(ax) {
        correct <- if (ax$angle[1] < node.pos[[i]]$angle[1]) {
          ax$angle[1] - node.pos[[i]]$angle[1] < -pi
        } else {
          ax$angle[1] - node.pos[[i]]$angle[1] < pi
        }
        if (correct) ax$node
      }))
      if (length(node_correction) != 0) {
        correct_edges <- E(graph)[node.pos[[i]]$node %--% node_correction]
        correct_edges_ends <- ends(graph, correct_edges, names = FALSE)
        new_node_ind <- correct_edges_ends %in% node.pos[[i]]$node
        correct_edges_ends[new_node_ind] <- extra_nodes$node[match(correct_edges_ends[new_node_ind], node.pos[[i]]$node)]
        eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(correct_edges))
        graph <- add_edges(graph, as.vector(t(correct_edges_ends)), attr = eattr)
        graph <- delete_edges(graph, as.numeric(correct_edges))
      }

      node.pos[[i]]$angle <- node.pos[[i]]$angle - split.angle / 2
      extra_nodes$angle <- extra_nodes$angle + split.angle / 2
      node.pos <- append(node.pos, list(extra_nodes))
    }
  }
  node.pos <- lapply(node.pos, function(nodes) {
    if (nrow(nodes) > 0) {
      nodes$x <- nodes$r * cos(nodes$angle)
      nodes$y <- nodes$r * sin(nodes$angle)
    }
    nodes
  })
  node.pos <- rbind_dfs(node.pos)
  node.pos <- node.pos[order(node.pos$node), names(node.pos) != 'node']
  extra_data <- as_tibble(as_tbl_graph(graph), active = 'nodes')
  node.pos <- cbind(node.pos, extra_data[, !names(extra_data) %in% names(node.pos), drop = FALSE])
  attr(node.pos, 'graph') <- as_tbl_graph(graph)
  node.pos
}
