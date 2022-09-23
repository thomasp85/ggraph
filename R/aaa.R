utils::globalVariables(c(
  'angle',
  'center_size',
  'circular',
  'con.id',
  'direction',
  'edge.id',
  'edge_x',
  'end',
  'from',
  'height',
  'r',
  'r0',
  'section',
  'start',
  'to',
  'width',
  'x',
  'xend',
  'xmin',
  'xmax',
  'y',
  'yend'
))

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatFilter <- ggproto('StatFilter', StatIdentity,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        cli::cli_abort('{.field filter} must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data
  },
  default_aes = aes(filter = TRUE)
)

aes_intersect <- function(aes1, aes2) {
  aes <- c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)])
  class(aes) <- 'uneval'
  aes
}

data_type <- function(data) {
  type <- attr(data, 'type_ggraph')
  if (is.null(type)) {
    if (inherits(data, 'layout_ggraph')) {
      return('node_ggraph')
    } else {
      return('other')
    }
  }
  type
}

# non-orderaltering version of make.unique
make_unique <- function(x, sep = '.') {
  if (!anyDuplicated(x)) return(x)
  groups <- match(x, unique0(x))
  suffix <- unsplit(lapply(split(x, groups), seq_along), groups)
  max_chars <- nchar(max(suffix))
  suffix_format <- paste0('%0', max_chars, 'd')
  paste0(x, sep, sprintf(suffix_format, suffix))
}

warn_dropped_vars <- function(layout, data) {
  overlap <- intersect(names(layout), names(data))
  if (length(overlap) > 0 && !identical(as.list(layout[overlap]), as.list(data[overlap]))) {
    cli::cli_warn('Existing variables {.var {overlap}} overwritten by layout variables')
  }
}

combine_layout_nodes <- function(layout, nodes) {
  warn_dropped_vars(layout, nodes)
  cbind(layout, nodes[, !names(nodes) %in% names(layout), drop = FALSE])
}

empty_data <- function(x) {
  length(x) == 0 || nrow(x) == 0
}

combine_aes <- function(aes1, aes2) {
  aes_all <- c(aes1[setdiff(names(aes1), names(aes2))], aes2)
  class(aes_all) <- class(aes1)
  aes_all
}
