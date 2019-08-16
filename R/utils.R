#' Get the angle of nodes and edges
#'
#' These helper functions makes it easy to calculate the angle associated with
#' nodes and edges. For nodes the angle is defined as the angle of the vector
#' pointing towards the node position, and is thus mainly suited for circular
#' layouts where it can be used to calculate the angle of labels. For edges it
#' is simply the angle of the vector describing the edge.
#'
#' @param x,y A vector of positions
#'
#' @param xend,yend The end position of the edge
#'
#' @param degrees Logical. Should the angle be returned in degree (`TRUE`)
#' or radians (`FALSE`). Defaults to `TRUE`.
#'
#' @return A vector with the angle of each node/edge
#'
#' @examples
#' require(tidygraph)
#' flareGraph <- tbl_graph(flare$vertices, flare$edges)
#'
#' ggraph(flareGraph, 'dendrogram', circular = TRUE) +
#'   geom_edge_diagonal0() +
#'   geom_node_text(aes(filter = leaf, angle = node_angle(x, y), label = shortName),
#'     hjust = 'outward', size = 2
#'   ) +
#'   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
#' @export
#'
node_angle <- function(x, y, degrees = TRUE) {
  angles <- atan2(y, x)
  angles[angles < 0] <- angles[angles < 0] + 2 * pi
  if (degrees) {
    angles * 360 / (2 * pi)
  } else {
    angles
  }
}
#' @rdname node_angle
#'
#' @export
edge_angle <- function(x, y, xend, yend, degrees = TRUE) {
  x <- xend - x
  y <- yend - y
  node_angle(x, y, degrees)
}


### COPY FROM GGPLOT2 NON-EXPORTS
#' @importFrom scales rescale_mid
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}
manual_scale <- function(aesthetic, values, ...) {
  pal <- function(n) {
    if (n > length(values)) {
      stop('Insufficient values in manual scale. ', n,
        ' needed but only ', length(values), ' provided.',
        call. = FALSE
      )
    }
    values
  }
  discrete_scale(aesthetic, 'manual', pal, ...)
}
#' @importFrom scales zero_range
resolution <- function(x, zero = TRUE) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE))) {
    return(1)
  }
  x <- unique(as.numeric(x))
  if (zero) {
    x <- unique(c(0, x))
  }
  min(diff(sort(x)))
}
'%||%' <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}
element_render <- function(theme, element, ..., name = NULL) {
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message('Theme element ', element, ' missing')
    return(zeroGrob())
  }
  ggname(paste(element, name, sep = '.'), element_grob(el, ...))
}
.all_aesthetics <- c(
  'adj', 'alpha', 'angle', 'bg', 'cex', 'col', 'color', 'colour',
  'fg', 'fill', 'group', 'hjust', 'label', 'linetype', 'lower',
  'lty', 'lwd', 'max', 'middle', 'min', 'pch', 'radius', 'sample',
  'shape', 'size', 'srt', 'upper', 'vjust', 'weight', 'width',
  'x', 'xend', 'xmax', 'xmin', 'xintercept', 'y', 'yend', 'ymax',
  'ymin', 'yintercept', 'z'
)
.base_to_ggplot <- structure(
  c(
    'colour', 'colour', 'shape', 'size', 'linetype', 'size', 'angle', 'hjust',
    'fill', 'colour', 'ymin', 'ymax'
  ),
  .Names = c(
    'col', 'color', 'pch', 'cex', 'lty', 'lwd', 'srt', 'adj', 'bg',
    'fg', 'min', 'max'
  )
)
rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]

  old_names <- match(names(x), names(.base_to_ggplot))
  names(x)[!is.na(old_names)] <- .base_to_ggplot[old_names[!is.na(old_names)]]

  x
}

pch_lookup <- c(
  "square open" = 0,
  "circle open" = 1,
  "triangle open" = 2,
  "plus" = 3,
  "cross" = 4,
  "diamond open" = 5,
  "triangle down open" = 6,
  "square cross" = 7,
  "asterisk" = 8,
  "diamond plus" = 9,
  "circle plus" = 10,
  "star" = 11,
  "square plus" = 12,
  "circle cross" = 13,
  "square triangle" = 14,
  "triangle square" = 14,
  "square" = 15,
  "circle small" = 16,
  "triangle" = 17,
  "diamond" = 18,
  "circle" = 19,
  "bullet" = 20,
  "circle filled" = 21,
  "square filled" = 22,
  "diamond filled" = 23,
  "triangle filled" = 24,
  "triangle down filled" = 25
)
translate_pch <- function(pch) {
  if (is.numeric(pch)) return(pch)
  if (!is.character(pch)) stop('Unknown shape format', call. = FALSE)
  if (nchar(pch[1]) <= 1) return(pch)

  pch <- charmatch(pch, names(pch_lookup))

  if (any(is.na(pch))) stop('Unknown shape name', call. = FALSE)
  if (any(pch == 0)) stop('Ambiguous shape name', call. = FALSE)

  pch_lookup[pch]
}

#' @importFrom viridis scale_color_viridis
#' @export
viridis::scale_color_viridis

#' @importFrom viridis scale_fill_viridis
#' @export
viridis::scale_fill_viridis

new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop('Elements must be named',
                                                call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop('Elements must equal the number of rows or 1',
                              call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- 'data.frame'

  attr(x, 'row.names') <- .set_row_names(n)
  x
}
df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}
split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}
# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}
split_indices <- function(group) {
  split(seq_along(group), group)
}
# Adapted from plyr:::id_vars
# Create a unique id for elements in a single vector
id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n <- 0L
  } else if (!is.null(attr(x, 'n')) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- length(levels(x))
  } else {
    levels <- sort(unique(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, 'n') <- n
  id
}
#' Create an unique integer id for each unique row in a data.frame
#'
#' Properties:
#' - `order(id)` is equivalent to `do.call(order, df)`
#' - rows containing the same data have the same value
#' - if `drop = FALSE` then room for all possibilites
#'
#' @param .variables list of variables
#' @param drop Should unused factor levels be dropped?
#'
#' @return An integer vector with attribute `n` giving the total number of
#' possible unique rows
#'
#' @keywords internal
#' @noRd
#'
id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- vapply(.variables, length, integer(1))
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, 'n') <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, 'n', FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- do.call('paste', c(ids, sep = '\r'))
    res <- match(char_id, unique(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- do.call('cbind', ids)
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  }
  else {
    res <- as.integer(res)
    attr(res, 'n') <- n
    res
  }
}
#' Bind data frames together by common column names
#'
#' This function is akin to `plyr::rbind.fill`, `dplyr::bind_rows`, and
#' `data.table::rbindlist`. It takes data frames in a list and stacks them on
#' top of each other, filling out values with `NA` if the column is missing from
#' a data.frame
#'
#' @param dfs A list of data frames
#'
#' @return A data.frame with the union of all columns from the data frames given
#' in `dfs`
#'
#' @keywords internal
#' @noRd
#'
rbind_dfs <- function(dfs) {
  out <- list()
  columns <- unique(unlist(lapply(dfs, names)))
  nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
  total <- sum(nrows)
  if (length(columns) == 0) return(new_data_frame(list(), total))
  allocated <- rep(FALSE, length(columns))
  names(allocated) <- columns
  col_levels <- list()
  for (df in dfs) {
    new_columns <- intersect(names(df), columns[!allocated])
    for (col in new_columns) {
      if (is.factor(df[[col]])) {
        all_factors <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.factor(val)
        }, logical(1)))
        if (all_factors) {
          col_levels[[col]] <- unique(
            unlist(lapply(dfs, function(df) levels(.subset2(df, col))))
          )
        }
        out[[col]] <- rep(NA_character_, total)
      } else {
        out[[col]] <- rep(.subset2(df, col)[1][NA], total)
      }
    }
    allocated[new_columns] <- TRUE
    if (all(allocated)) break
  }
  pos <- c(cumsum(nrows) - nrows + 1)
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    rng <- seq(pos[i], length.out = nrows[i])
    for (col in names(df)) {
      if (inherits(df[[col]], 'factor')) {
        out[[col]][rng] <- as.character(df[[col]])
      } else {
        out[[col]][rng] <- df[[col]]
      }
    }
  }
  for (col in names(col_levels)) {
    out[[col]] <- factor(out[[col]], levels = col_levels[[col]])
  }
  attributes(out) <- list(
    class = 'data.frame',
    names = names(out),
    row.names = .set_row_names(total)
  )
  out
}
