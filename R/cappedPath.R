#' @importFrom grid grob is.unit unit gTree
cappedPathGrob <- function(x, y, id = NULL, id.lengths = NULL, arrow = NULL,
                           start.cap = NULL, start.cap2 = NULL, start.captype = 'circle',
                           end.cap = NULL, end.cap2 = NULL, end.captype = 'circle',
                           default.units = 'npc', name = NULL, gp = gpar(), vp = NULL, constant = TRUE) {
  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }
  if (is.null(id)) {
    if (!is.null(id.lengths)) {
      id <- rep(seq_along(id.lengths), id.lengths)
    } else {
      id <- rep(1, length(x))
    }
  }
  n <- length(unique(id))
  start.cap <- validate_cap(start.cap, default.units, n)
  if (is.null(start.cap2)) {
    start.cap2 <- start.cap
  } else {
    start.cap2 <- validate_cap(start.cap2, default.units, n)
  }
  end.cap <- validate_cap(end.cap, default.units, n)
  if (is.null(end.cap2)) {
    end.cap2 <- end.cap
  } else {
    end.cap2 <- validate_cap(end.cap2, default.units, n)
  }
  if (!all(c(start.captype, end.captype) %in% c('circle', 'rect'))) {
    stop('captype must be either `circle` or `rect`', call. = FALSE)
  }
  start.captype <- rep(start.captype, length.out = n)
  end.captype <- rep(end.captype, length.out = n)

  n_points <- length(x)
  group_diff <- id[-1] != id[-n_points]
  start <- c(TRUE, group_diff)
  end <- c(group_diff, TRUE)

  if (constant) {
    gp <- lapply(gp, function(par) {
      if (length(par) == length(end)) {
        par[start]
      } else {
        par
      }
    })
  } else {
    gp <- lapply(gp, function(par) {
      if (length(par) == length(end)) {
        par[!end]
      } else {
        par
      }
    })
  }
  class(gp) <- 'gpar'

  if (is.null(start.cap) && is.null(end.cap)) {
    if (constant) {
      grob(
        x = x, y = y, id = id, id.lengths = NULL, arrow = arrow,
        name = name, gp = gp, vp = vp, cl = 'polyline'
      )
    } else {
      grob(
        x0 = x[!end], y0 = y[!end], x1 = x[!start], y1 = y[!start], id = id[!end],
        arrow = arrow, name = name, gp = gp, vp = vp, cl = 'segments'
      )
    }
  } else {
    gTree(
      x = x, y = y, id = id, arrow = arrow, constant = constant, start = start, end = end,
      start.cap = start.cap, start.cap2 = start.cap2, start.captype = start.captype,
      end.cap = end.cap, end.cap2 = end.cap2, end.captype = end.captype,
      name = name, gp = gp, vp = vp, cl = 'cappedpathgrob'
    )
  }
}
#' Dynamic capping of paths
#'
#' This function takes care of updating which parts of the paths get removed
#' when the device dimensions are updated.
#'
#' @importFrom grid convertX convertY convertWidth convertHeight unit grob makeContent setChildren gList
#' @export
#' @keywords internal
makeContent.cappedpathgrob <- function(x) {
  x_new <- convertX(x$x, 'mm', TRUE)
  y_new <- convertY(x$y, 'mm', TRUE)
  start.cap <- convertWidth(x$start.cap, 'mm', TRUE)
  end.cap <- convertWidth(x$end.cap, 'mm', TRUE)
  start.cap2 <- convertHeight(x$start.cap2, 'mm', TRUE)
  end.cap2 <- convertHeight(x$end.cap2, 'mm', TRUE)
  truncated <- cut_lines(
    x_new, y_new, as.integer(x$id), start.cap, start.cap2,
    end.cap, end.cap2, x$start.captype, x$end.captype
  )
  keep <- !is.na(truncated$x)
  if (!any(keep)) {
    lines <- zeroGrob()
  } else if (x$constant) {
    x_new <- truncated$x[keep]
    y_new <- truncated$y[keep]
    id <- x$id[keep]
    all_id <- unique(id)
    gp <- lapply(x$gp, function(par) {
      if (length(par) == 1) {
        par
      } else {
        par[all_id]
      }
    })
    class(gp) <- 'gpar'
    id <- match(id, all_id)
    lines <- grob(
      x = unit(x_new, 'mm'), y = unit(y_new, 'mm'), id = id,
      id.lengths = NULL, arrow = x$arrow, name = x$name, gp = gp,
      vp = x$vp, cl = 'polyline'
    )
  } else {
    x0 <- truncated$x[!x$end]
    y0 <- truncated$y[!x$end]
    x1 <- truncated$x[!x$start]
    y1 <- truncated$y[!x$start]
    lines <- grob(
      x0 = unit(x0, 'mm'), y0 = unit(y0, 'mm'), x1 = unit(x1, 'mm'), id = x$id[!x$end],
      y1 = unit(y1, 'mm'), arrow = x$arrow, name = x$name, gp = x$gp,
      vp = x$vp, cl = 'segments'
    )
  }
  setChildren(x, gList(lines))
}
#' @importFrom grid is.unit unit
validate_cap <- function(cap, default.units, n) {
  if (is.null(cap)) {
    return()
  }
  if (!is.unit(cap)) cap <- unit(cap, default.units)
  rep(cap, length.out = n)
}
