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
#' `centerSize`, `split`, `axis`, `section`, `angle`,
#' `circular` as well as any information stored as node variables in the
#' tbl_graph object.
#'
#' @references
#' Krzywinski, M., Birol, I., Jones, SJM., and Marra, MA. (2012). *Hive
#' plots-rational approach to visualizing networks*. Brief Bioinform 13 (5):
#' 627-644. <http://doi.org/10.1093/bib/bbr069>
#'
#' <http://www.hiveplot.net>
#'
#' @family layout_tbl_graph_*
#'
#' @importFrom igraph gorder vertex_attr gsize induced_subgraph add_vertices E ends add_edges delete_edges %--% edge_attr
#' @importFrom utils tail
layout_tbl_graph_hive <- function(graph, axis, axis.pos = NULL, sort.by = NULL, divide.by = NULL, divide.order = NULL, normalize = TRUE, center.size = 0.1, divide.size = 0.05, use.numeric = FALSE, offset = pi/2, split.axes = 'none', split.angle = pi/6, circular = FALSE) {
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
      warning("Number of axes not matching axis.pos argument. Recycling as needed")
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
    normalizeTo <- rep(1, length(axes))
  } else {
    normalizeTo <- lengths(axes) / max(lengths(axes))
  }
  node.pos <- Map(function(nodes, axisLength, axis, angle) {
    splitAxis <- switch(
      split.axes,
      all = TRUE,
      loops = gsize(induced_subgraph(graph, nodes)) > 0,
      none = FALSE,
      stop('Unknown split argument. Use "all", "loops" or "none"')
    )
    nodeDiv <- axisLength / length(nodes)
    if (is.null(divide.by)) {
      nodeSplit <- list(`1` = nodes)
    } else {
      if (use.numeric) {
        stop('Cannot divide axis while use.numeric = TRUE')
      }
      nodeSplit <- split(nodes, divide.by[nodes])
      if (!is.null(divide.order)) {
        if (!all(divide.order %in% names(nodeSplit))) {
          stop('All ', divide.by, ' levels must be present in divide.order')
        }
        nodeSplit <- nodeSplit[order(match(names(nodeSplit), divide.order))]
      }
    }
    nodePos <- lapply(nodeSplit, function(nodes) {
      if (length(nodes) == 0) return(numeric())
      if (is.null(sort.by)) {
        pos <- match(seq_along(nodes), order(nodes)) - 1
        pos <- pos * nodeDiv
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
              pos <- (pos - min(pos))/diff(range(pos))
            }
          } else {
            pos <- (pos - numeric.range[1])/diff(numeric.range)
          }
        } else {
          pos <- match(seq_along(pos), order(pos)) - 1
          pos <- pos * nodeDiv
        }
      }
      pos
    })
    nodePos <- Reduce(function(l, r) {
      append(l, list(r + nodeDiv + divide.size + max(l[[length(l)]])))
    }, x = nodePos[-1], init = nodePos[1])
    nodePos <- unlist(nodePos) + center.size
    data.frame(
      node = nodes,
      r = nodePos[match(nodes, unlist(nodeSplit))],
      centerSize = center.size,
      split = splitAxis,
      axis = axis,
      section = rep(names(nodeSplit), lengths(nodeSplit))[match(nodes, unlist(nodeSplit))],
      angle = angle,
      circular = FALSE,
      stringsAsFactors = FALSE
    )
  }, nodes = axes, axisLength = normalizeTo, axis = names(axes), angle = axis.pos)
  for (i in seq_along(node.pos)) {
    if (node.pos[[i]]$split[1]) {
      nNewNodes <- nrow(node.pos[[i]])
      newNodeStart <- gorder(graph) + 1
      extraNodes <- node.pos[[i]]
      extraNodes$node <- seq(newNodeStart, length.out = nNewNodes)
      vattr <- lapply(vertex_attr(graph), `[`, i = node.pos[[i]]$node)
      graph  <- add_vertices(graph, nNewNodes, attr = vattr)

      loopEdges <- E(graph)[node.pos[[i]]$node %--% node.pos[[i]]$node]
      if (length(loopEdges) != 0) {
        loopEdgesEnds <- ends(graph, loopEdges, names = FALSE)
        correctOrderEnds <- node.pos[[i]]$r[match(loopEdgesEnds[,1], node.pos[[i]]$node)] <
          node.pos[[i]]$r[match(loopEdgesEnds[,2], node.pos[[i]]$node)]
        loopEdgesEnds <- data.frame(
          from = ifelse(correctOrderEnds, loopEdgesEnds[,1], loopEdgesEnds[,2]),
          to = ifelse(correctOrderEnds, loopEdgesEnds[,2], loopEdgesEnds[,1])
        )
        loopEdgesEnds$to <- extraNodes$node[match(loopEdgesEnds$to, node.pos[[i]]$node)]
        loopEdgesEnds <- matrix(c(
          ifelse(correctOrderEnds, loopEdgesEnds$from, loopEdgesEnds$to),
          ifelse(correctOrderEnds, loopEdgesEnds$to, loopEdgesEnds$from)
        ), nrow = 2, byrow = TRUE)
        eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(loopEdges))
        graph <- add_edges(graph, as.vector(loopEdgesEnds), attr = eattr)
        graph <- delete_edges(graph, as.numeric(loopEdges))
      }

      nodeCorrection <- unlist(lapply(node.pos[-i], function(ax) {
        correct <- if (ax$angle[1] < node.pos[[i]]$angle[1]) {
          ax$angle[1] - node.pos[[i]]$angle[1] < -pi
        } else {
          ax$angle[1] - node.pos[[i]]$angle[1] < pi
        }
        if (correct) ax$node
      }))
      if (length(nodeCorrection) != 0) {
        correctEdges <- E(graph)[node.pos[[i]]$node %--% nodeCorrection]
        correctEdgesEnds <- ends(graph, correctEdges, names = FALSE)
        newNodeInd <- correctEdgesEnds %in% node.pos[[i]]$node
        correctEdgesEnds[newNodeInd] <- extraNodes$node[match(correctEdgesEnds[newNodeInd], node.pos[[i]]$node)]
        eattr <- lapply(edge_attr(graph), `[`, i = as.numeric(correctEdges))
        graph <- add_edges(graph, as.vector(t(correctEdgesEnds)), attr = eattr)
        graph <- delete_edges(graph, as.numeric(correctEdges))
      }

      node.pos[[i]]$angle <- node.pos[[i]]$angle - split.angle/2
      extraNodes$angle <- extraNodes$angle + split.angle/2
      node.pos <- append(node.pos, list(extraNodes))
    }
  }
  node.pos <- lapply(node.pos, function(nodes) {
    nodes$x <- nodes$r * cos(nodes$angle)
    nodes$y <- nodes$r * sin(nodes$angle)
    nodes
  })
  node.pos <- do.call(rbind, node.pos)
  node.pos <- node.pos[order(node.pos$node), names(node.pos) != 'node']
  extraData <- as_tibble(as_tbl_graph(graph), active = 'nodes')
  node.pos <- cbind(node.pos, extraData[, !names(extraData) %in% names(node.pos), drop = FALSE])
  attr(node.pos, 'graph') <- as_tbl_graph(graph)
  node.pos
}
