#' Create a grid of small multiples by node and/or edge attributes
#'
#' This function is equivalent to [ggplot2::facet_grid()] in that it
#' allows for building a grid of small multiples where rows and columns
#' correspond to a specific data value. While [ggplot2::facet_grid()]
#' could be used it would lead to unexpected results as it is not possible to
#' specify whether you are referring to a node or an edge attribute. Furthermore
#' [ggplot2::facet_grid()] will draw edges in panels even though the
#' panel does not contain both terminal nodes. `facet_graph` takes care of
#' all of these issues, allowing you to define which data type the rows and
#' columns are referencing as well as filtering the edges based on the nodes in
#' each panel (even when nodes are not drawn).
#'
#' @param row_type,col_type Either `'node'` or `'edge'`. Which data
#' type is being facetted in the rows and columns. Default is to facet on nodes
#' column wise and on edges row wise.
#'
#' @inheritParams ggplot2::facet_grid
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
#'   facet_graph(year ~ popularity)
#' @export
#'
facet_graph <- function(facets, row_type = 'edge', col_type = 'node',
                        margins = FALSE, scales = 'fixed', space = 'fixed',
                        shrink = TRUE, labeller = 'label_value', as.table = TRUE,
                        switch = NULL, drop = TRUE) {
  facet <- facet_grid(facets,
    margins = margins, scales = scales,
    space = space, shrink = shrink, labeller = labeller,
    as.table = as.table, switch = switch, drop = drop
  )
  seed <- sample(.Machine$integer.max, 1L)
  facet$params$rows[] <- lapply(seq_along(facet$params$rows), function(i) {
    rlang::quo(withr::with_seed(seed, !!facet$params$rows[[i]]))
  })
  facet$params$cols[] <- lapply(seq_along(facet$params$cols), function(i) {
    rlang::quo(withr::with_seed(seed, !!facet$params$cols[[i]]))
  })

  ggproto(NULL, FacetGraph,
    shrink = shrink,
    params = c(facet$params, list(
      row_type = row_type,
      col_type = col_type
    ))
  )
}

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetGraph <- ggproto('FacetGraph', FacetGrid,
  compute_layout = function(data, params) {
    plot_data <- data[[1]]
    data <- split(data, vapply(data, data_type, character(1)))

    rows <- params$rows
    cols <- params$cols
    row_data <- switch(
      params$row_type,
      node = data$node_ggraph,
      edge = data$edge_ggraph,
      stop('row_type must be either "node" or "edge"', call. = FALSE)
    )
    col_data <- switch(
      params$col_type,
      node = data$node_ggraph,
      edge = data$edge_ggraph,
      stop('col_type must be either "node" or "edge"', call. = FALSE)
    )

    base_rows <- combine_vars(row_data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
      base_rows[] <- lapply(base_rows, rev_order)
    }
    base_cols <- combine_vars(col_data, params$plot_env, cols, drop = params$drop)
    base <- df.grid(
      as.data.frame(base_rows, stringsAsFactors = FALSE),
      as.data.frame(base_cols, stringsAsFactors = FALSE)
    )

    # Add margins
    base <- reshape_add_margins(base, list(names(rows), names(cols)), params$margins)
    base <- unique(base)

    # Create panel info dataset
    panel <- id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, 'n')))

    rows <- if (is.null(names(rows))) 1L else id(base[names(rows)], drop = TRUE)
    cols <- if (is.null(names(cols))) 1L else id(base[names(cols)], drop = TRUE)

    panels <- new_data_frame(c(list(
      PANEL = panel, ROW = rows, COL = cols), base
    ))
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    panels$SCALE_X <- if (params$free$x) panels$COL else 1L
    panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

    node_placement <- if (nrow(base_cols) == 0) {
      list(`1` = plot_data$ggraph_index)
    } else {
      if (params$row_type == 'edge') params$rows <- NULL
      if (params$col_type == 'edge') params$cols <- NULL
      node_map <- FacetGrid$map_data(plot_data, panels, params)
      node_map <- expand_facet_map(node_map, panels)
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
        if (params$row_type == 'node') params$rows <- NULL
        if (params$col_type == 'node') params$cols <- NULL
        edge_map <- FacetGrid$map_data(data, layout, params)
        edge_map <- expand_facet_map(edge_map, layout)
        edge_map <- Map(function(map, nodes) {
          map[map$from %in% nodes & map$to %in% nodes, , drop = FALSE]
        }, map = split(edge_map, edge_map$PANEL), nodes = attr(layout, 'node_placement'))
        rbind_dfs(edge_map)
      },
      node_ggraph = {
        node_map <- lapply(attr(layout, 'node_placement'), function(nodes) {
          data[data$.ggraph.index %in% nodes, , drop = FALSE]
        })
        panel <- rep(seq_along(node_map), vapply(node_map, nrow, numeric(1)))
        node_map <- rbind_dfs(node_map)
        node_map$PANEL <- factor(panel, levels = levels(layout$PANEL))
        node_map
      }, {
        FacetGrid$map_data(data, layout, params)
      }
    )
  }
)

expand_facet_map <- function(map, layout) {
  ncols <- max(layout$COL)
  nrows <- max(layout$ROW)
  if (!(ncols == 1 && nrows == 1)) {
    map$PANEL[map$PANEL == -1] <- 1
  }
  if (ncols != 1) {
    data_cols <- unique(layout$COL[layout$PANEL %in% map$PANEL])
    if (length(data_cols) == 1) {
      map <- lapply(split(map, as.integer(map$PANEL)), function(data) {
        row <- layout$ROW[layout$PANEL == data$PANEL[1]]
        panels <- layout$PANEL[layout$ROW == row]
        data_expand <- data[rep(seq_len(nrow(data)), length(panels)), , drop = FALSE]
        data_expand$PANEL <- rep(panels, each = nrow(data))
        data_expand
      })
      map <- rbind_dfs(map)
    }
  }
  if (nrows != 1) {
    data_rows <- unique(layout$ROW[layout$PANEL %in% map$PANEL])
    if (length(data_rows) == 1) {
      map <- lapply(split(map, as.integer(map$PANEL)), function(data) {
        col <- layout$COL[layout$PANEL == data$PANEL[1]]
        panels <- layout$PANEL[layout$COL == col]
        data_expand <- data[rep(seq_len(nrow(data)), length(panels)), , drop = FALSE]
        data_expand$PANEL <- rep(panels, each = nrow(data))
        data_expand
      })
      map <- rbind_dfs(map)
    }
  }
  map$PANEL <- factor(map$PANEL, levels = levels(layout$PANEL))
  map
}
df.grid <- function(a, b) {
  if (is.null(a) || nrow(a) == 0) {
    return(b)
  }
  if (is.null(b) || nrow(b) == 0) {
    return(a)
  }
  indexes <- expand.grid(i_a = seq_len(nrow(a)), i_b = seq_len(nrow(b)))
  grid <- cbind(a[indexes$i_a, , drop = FALSE], b[indexes$i_b, , drop = FALSE])
  attr(grid, 'row.names') <- .set_row_names(nrow(grid))
  grid
}
ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique(x))
  }
}
