#' Draw edges as elbows
#'
#' This geom draws edges as an angle in the same manner as known from classic
#' dendrogram plots of hierarchical clustering results. In case a circular
#' transformation has been applied the first line segment will be drawn as an
#' arc as expected. This geom is only applicable to layouts that return a
#' direction for the edges (currently [layout_tbl_graph_dendrogram()],
#' [layout_tbl_graph_partition()] and
#' [layout_tbl_graph_igraph()] with the `"tree"` algorithm).
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_elbow` and `geom_edge_elbow0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **circular**
#' - **direction**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_elbow2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **circular**
#' - **direction**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_elbow` and `geom_edge_elbow2` furthermore takes the following
#' aesthetics.
#'
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#' @param strength How bend the elbow should be. 1 will give a right angle,
#' while `0` will give a straight line. Ignored for circular layouts
#' @inheritParams geom_edge_diagonal
#'
#' @author Thomas Lin Pedersen
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' irisDen <- hclust(dist(iris[1:4], method = 'euclidean'), method = 'ward.D2') %>%
#'   as_tbl_graph() %>%
#'   mutate(class = sample(letters[1:3], n(), TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(class = sample(letters[1:3], n(), TRUE))
#'
#' ggraph(irisDen, 'dendrogram', circular = TRUE) +
#'   geom_edge_elbow(aes(alpha = stat(index)))
#'
#' ggraph(irisDen, 'dendrogram') +
#'   geom_edge_elbow2(aes(colour = node.class))
#'
#' ggraph(irisDen, 'dendrogram', height = height) +
#'   geom_edge_elbow0(aes(colour = class))
#' @rdname geom_edge_elbow
#' @name geom_edge_elbow
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce radial_trans
#' @export
StatEdgeElbow <- ggproto('StatEdgeElbow', Stat,
  compute_panel = function(data, scales, flipped = FALSE, n = 100, strength = 1) {
    data$group <- make_unique(data$group)
    if (data$circular[1] && n %% 2 == 1) {
      n <- n + 1
    }
    if (!data$circular[1] && n %% 2 == 0) {
      n <- n + 1
    }
    index <- seq(0, 1, length.out = n)
    if (any(data$circular)) {
      if (strength != 1) warning('strength is ignored for circular elbow edges', call. = FALSE)
      circ_id <- which(data$circular)
      data_circ <- data[circ_id, ]
      radial <- radial_trans(c(0, 1), c(2 * pi, 0), pad = 0, offset = 0)
      start <- atan2(data_circ$y, data_circ$x)
      radii_start <- sqrt(data_circ$x^2 + data_circ$y^2)
      radii_end <- sqrt(data_circ$xend^2 + data_circ$yend^2)
      angel_diff <- (data_circ$x * data_circ$xend + data_circ$y * data_circ$yend) /
        (radii_start * radii_end)
      angel_diff[is.nan(angel_diff)] <- 0
      angel_diff <- suppressWarnings(acos(angel_diff))
      angel_diff[is.nan(angel_diff)] <- 0
      end <- start + ifelse(data_circ$direction == 'left',
        -angel_diff, angel_diff
      )
      angles <- unlist(Map(seq, from = start, to = end, length.out = n / 2))
      radii <- rep(sqrt(data$y[circ_id]^2 + data$x[circ_id]^2), each = n / 2)
      path_circ <- radial$transform(r = radii, a = angles)
      path_circ$.orig_index <- rep(circ_id, each = n / 2)
      path_circ$group <- rep(data_circ$group, each = n / 2)
      path_circ$index <- rep(index[seq_len(n / 2)], length(circ_id))
      radii_rel <- radii_start / radii_end
      elbow_x <- data_circ$xend * radii_rel
      elbow_y <- data_circ$yend * radii_rel
      elbow_x <- unlist(Map(seq,
        from = elbow_x, to = data_circ$xend,
        length.out = n / 2
      ))
      elbow_y <- unlist(Map(seq,
        from = elbow_y, to = data_circ$yend,
        length.out = n / 2
      ))
      path_circ <- rbind_dfs(list(
        path_circ,
        new_data_frame(list(
          x = elbow_x,
          y = elbow_y,
          .orig_index = path_circ$.orig_index,
          group = path_circ$group,
          index = rep(
            index[seq_len(n / 2) + n / 2],
            length(circ_id)
          )
        ))
      ))
      path_circ <- cbind(path_circ, data[path_circ$.orig_index, !names(data) %in%
        c('x', 'y', 'xend', 'yend', 'group')])
      path_circ$.orig_index <- NULL
    }
    if (any(!data$circular)) {
      path_lin <- lapply(which(!data$circular), function(i) {
        if (flipped) {
          path <- new_data_frame(list(
            x = approx(c(data$x[i], data$xend[i] + (data$x[i] - data$xend[i]) * strength, data$xend[i]),
              n = n
            )$y,
            y = approx(c(data$y[i], data$yend[i], data$yend[i]),
              n = n
            )$y,
            group = data$group[i],
            index = index
          ))
        } else {
          path <- new_data_frame(list(
            x = approx(c(data$x[i], data$xend[i], data$xend[i]),
              n = n
            )$y,
            y = approx(c(data$y[i], data$yend[i] + (data$y[i] - data$yend[i]) * strength, data$yend[i]),
              n = n
            )$y,
            group = data$group[i],
            index = index
          ))
        }
        cbind(path, data[rep(i, nrow(path)), !names(data) %in%
          c('x', 'y', 'xend', 'yend', 'group')])
      })
      path_lin <- rbind_dfs(path_lin)

      if (any(data$circular)) {
        paths <- rbind_dfs(list(path_lin, path_circ))
      } else {
        paths <- path_lin
      }
    } else {
      paths <- path_circ
    }
    paths[order(paths$group), ]
  },
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    data
  },
  default_aes = aes(filter = TRUE),
  required_aes = c('x', 'y', 'xend', 'yend', 'circular', 'direction')
)
#' @rdname geom_edge_elbow
#'
#' @export
geom_edge_elbow <- function(mapping = NULL, data = get_edges(),
                            position = 'identity', arrow = NULL, strength = 1,
                            flipped = FALSE, n = 100, lineend = 'butt',
                            linejoin = 'round', linemitre = 1,
                            label_colour = 'black', label_alpha = 1,
                            label_parse = FALSE, check_overlap = FALSE,
                            angle_calc = 'rot', force_flip = TRUE,
                            label_dodge = NULL, label_push = NULL,
                            show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular, direction = direction, group = edge.id
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeElbow,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = FALSE, flipped = flipped, strength = strength,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeElbow2 <- ggproto('StatEdgeElbow2', Stat,
  compute_panel = function(data, scales, flipped = FALSE, n = 100, strength = 1) {
    pos_cols <- c('x', 'y', 'group', 'circular', 'direction', 'PANEL')
    data <- data[order(data$group), ]
    pos_data <- cbind(data[c(TRUE, FALSE), pos_cols], data[
      c(FALSE, TRUE),
      c('x', 'y')
    ])
    pos_data$group <- make_unique(pos_data$group)
    names(pos_data) <- c(pos_cols, 'xend', 'yend')
    new_data <- StatEdgeElbow$compute_panel(pos_data, scales, flipped, n, strength)
    extra_cols <- !names(data) %in% pos_cols
    index <- match(pos_data$group, new_data$group)
    index <- as.vector(matrix(c(index, index + 1), nrow = 2, byrow = T))
    new_data$.interp <- TRUE
    new_data$.interp[index] <- FALSE
    if (sum(extra_cols) != 0) {
      for (i in names(data)[extra_cols]) {
        new_data[[i]] <- NA
        new_data[[i]][index] <- data[[i]]
      }
    }
    new_data
  },
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop2(data)
    if (nrow(data) == 0) return(NULL)
    data
  },
  default_aes = aes(filter = TRUE),
  required_aes = c('x', 'y', 'group', 'circular', 'direction')
)
#' @rdname geom_edge_elbow
#'
#' @export
geom_edge_elbow2 <- function(mapping = NULL, data = get_edges('long'),
                             position = 'identity', arrow = NULL, strength = 1,
                             flipped = FALSE, n = 100, lineend = 'butt',
                             linejoin = 'round', linemitre = 1,
                             label_colour = 'black', label_alpha = 1,
                             label_parse = FALSE, check_overlap = FALSE,
                             angle_calc = 'rot', force_flip = TRUE,
                             label_dodge = NULL, label_push = NULL,
                             show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, group = edge.id,
    circular = circular, direction = direction
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeElbow2,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        linemitre = linemitre, na.rm = FALSE, n = n,
        interpolate = TRUE, flipped = flipped, strength = strength,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}
#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatEdgeElbow0 <- ggproto('StatEdgeElbow0', Stat,
  compute_panel = function(data, scales, flipped = FALSE, strength = 1) {
    data$group <- make_unique(data$group)
    if (any(data$circular)) {
      if (strength != 1) warning('strength is ignored for circular elbow edges', call. = FALSE)
      circ_id <- which(data$circular)
      data_circ <- data[circ_id, ]
      radial <- radial_trans(c(0, 1), c(2 * pi, 0), pad = 0, offset = 0)
      start <- atan2(data_circ$y, data_circ$x)
      angel_diff <- (data_circ$x * data_circ$xend + data_circ$y * data_circ$yend) /
        (sqrt(data_circ$x^2 + data_circ$y^2) *
          sqrt(data_circ$xend^2 + data_circ$yend^2))
      angel_diff[is.nan(angel_diff)] <- 0
      angel_diff <- suppressWarnings(acos(angel_diff))
      angel_diff[is.nan(angel_diff)] <- 0
      end <- start + ifelse(data_circ$direction == 'left',
        -angel_diff, angel_diff
      )
      angles <- unlist(Map(seq, from = start, to = end, length.out = 50))
      radii <- rep(sqrt(data$y[circ_id]^2 + data$x[circ_id]^2), each = 50)
      path_circ <- radial$transform(r = radii, a = angles)
      path_circ$.orig_index <- rep(circ_id, each = 50)
      path_circ$group <- rep(data_circ$group, each = 50)
      path_circ <- rbind_dfs(list(
        path_circ,
        new_data_frame(list(
          x = data$xend[circ_id],
          y = data$yend[circ_id],
          .orig_index = circ_id,
          group = data_circ$group
        ))
      ))
      path_circ <- cbind(path_circ, data[path_circ$.orig_index, !names(data) %in%
        c('x', 'y', 'xend', 'yend')])
      path_circ$.orig_index <- NULL
    }
    if (any(!data$circular)) {
      path_lin <- lapply(which(!data$circular), function(i) {
        if (flipped) {
          path <- new_data_frame(list(
            x = c(data$x[i], data$xend[i] + (data$x[i] - data$xend[i]) * strength, data$xend[i]),
            y = c(data$y[i], data$yend[i], data$yend[i]),
            group = data$group
          ))
        } else {
          path <- new_data_frame(list(
            x = c(data$x[i], data$xend[i], data$xend[i]),
            y = c(data$y[i], data$yend[i] + (data$y[i] - data$yend[i]) * strength, data$yend[i]),
            group = data$group[i]
          ))
        }
        cbind(path, data[rep(i, nrow(path)), !names(data) %in%
          c('x', 'y', 'xend', 'yend')])
      })
      path_lin <- rbind_dfs(path_lin)

      if (any(data$circular)) {
        paths <- rbind_dfs(list(path_lin, path_circ))
      } else {
        paths <- path_lin
      }
    } else {
      paths <- path_circ
    }
    paths[order(paths$group), ]
  },
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    if (nrow(data) == 0) return(NULL)
    data
  },
  default_aes = aes(filter = TRUE),
  required_aes = c('x', 'y', 'xend', 'yend', 'circular', 'direction')
)
#' @rdname geom_edge_elbow
#'
#' @export
geom_edge_elbow0 <- function(mapping = NULL, data = get_edges(),
                             position = 'identity', arrow = NULL, flipped = FALSE,
                             lineend = 'butt', show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend,
    circular = circular, direction = direction
  ))
  layer(
    data = data, mapping = mapping, stat = StatEdgeElbow0,
    geom = GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list(
        arrow = arrow, lineend = lineend, na.rm = FALSE,
        interpolate = FALSE, flipped = flipped, ...
      )
    )
  )
}
