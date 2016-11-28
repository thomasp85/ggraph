#' @include aaa.R
NULL

FacetGraph <- ggproto('FacetGraph', FacetGrid,
    compute_layout = function(data, params) {
        plot_data <- data[[1]]
        data <- split(data, sapply(data, dataType))

        edges <- plyr::as.quoted(params$edges)
        nodes <- plyr::as.quoted(params$nodes)

        base_rows <- combine_vars(data$edge_ggraph, params$plot_env, edges, drop = params$drop)
        base_cols <- combine_vars(data$node_ggraph, params$plot_env, nodes, drop = params$drop)
        base <- df.grid(base_rows, base_cols)

        # Add margins
        base <- reshape2::add_margins(base, list(names(edges), names(nodes)), params$margins)
        # Work around bug in reshape2
        base <- unique(base)

        # Create panel info dataset
        panel <- plyr::id(base, drop = TRUE)
        panel <- factor(panel, levels = seq_len(attr(panel, "n")))

        rows <- if (is.null(names(edges))) 1L else plyr::id(base[names(edges)], drop = TRUE)
        cols <- if (is.null(names(nodes))) 1L else plyr::id(base[names(nodes)], drop = TRUE)

        panels <- data.frame(PANEL = panel, ROW = rows, COL = cols, base,
                             check.names = FALSE, stringsAsFactors = FALSE)
        panels <- panels[order(panels$PANEL), , drop = FALSE]
        rownames(panels) <- NULL

        panels$SCALE_X <- if (params$free$x) panels$COL else 1L
        panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

        node_placement <- if (nrow(base_cols) == 0) {
            rep(1, nrow(plot_data))
        } else {
            node_map <-FacetGrid$map_data(plot_data, panels, modifyList(params, list(cols = params$nodes)))
            node_map$PANEL[order(node_map$ggraph.index)]
        }

        structure(panels, node_placement = node_placement)
    },
    map_data = function(data, layout, params) {
        switch(
            dataType(data),
            edge_ggraph = {
                node_loc <- attr(layout, 'node_placement')
                data_sub <- data[node_loc[data$from] == node_loc[data$to], ]
                data_sub$ggraph.node <- node_loc[data_sub$from]
                layout$ggraph.node <- layout$COL
                FacetGrid$map_data(data_sub, layout, modifyList(params, list(cols = plyr::as.quoted('ggraph.node'), rows = params$edges)))
            },
            node_ggraph = {
                layout$ggraph.edge <- layout$ROW
                data_sub <- modifyList(data, list(ggraph.edge = NULL))
                FacetGrid$map_data(data_sub, layout, modifyList(params, list(cols = params$nodes, rows = plyr::as.quoted('ggraph.edge'))))
            },
            {
                layout$ggraph.edge <- layout$ROW
                layout$ggraph.node <- layout$COL
                data_sub <- modifyList(data, list(ggraph.edge = NULL, ggraph.node = NULL))
                FacetGrid$map_data(data_sub, layout, modifyList(params, list(cols = plyr::as.quoted('ggraph.node'), rows = plyr::as.quoted('ggraph.edge'))))
            }
        )
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
        params <- modifyList(params, list(rows = params$edges, cols = params$nodes))
        FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    }
)
#' @export
facet_graph <- function(nodes = ., edges = ., margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", switch = NULL, drop = TRUE) {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
        x = any(scales %in% c("free_x", "free")),
        y = any(scales %in% c("free_y", "free"))
    )

    space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
    space_free <- list(
        x = any(space %in% c("free_x", "free")),
        y = any(space %in% c("free_y", "free"))
    )

    if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
        stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
    }

    nodes <- plyr::as.quoted(substitute(nodes))
    nodes <- nodes[!sapply(nodes, identical, as.name("."))]
    edges <- plyr::as.quoted(substitute(edges))
    edges <- edges[!sapply(edges, identical, as.name("."))]
    if (length(nodes) + length(edges) == 0) {
        stop("Must facet by at least nodes or edges", call. = FALSE)
    }

    ggproto(NULL, FacetGraph,
            shrink = shrink,
            params = list(nodes = nodes, edges = edges, margins = margins,
                          free = free, space_free = space_free, labeller = labeller,
                          switch = switch, drop = drop)
    )
}

df.grid <- function (a, b) {
    if (is.null(a) || nrow(a) == 0)
        return(b)
    if (is.null(b) || nrow(b) == 0)
        return(a)
    indexes <- expand.grid(i_a = seq_len(nrow(a)), i_b = seq_len(nrow(b)))
    plyr::unrowname(cbind(a[indexes$i_a, , drop = FALSE], b[indexes$i_b,
                                                            , drop = FALSE]))
}
