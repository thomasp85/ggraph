#' @export
#' @keywords internal
makeContent.parallelPath <- function(x) {
  will_cap <- inherits(x, 'cappedpathgrob')
  if (!is.null(x$id)) {
    sep <- x$sep[!duplicated(x$id)]
  }
  x <- NextMethod()

  if (will_cap) {
    x$children[[1]]$sep <- sep[x$children[[1]]$id]
    if (inherits(x$children[[1]], 'segments')) {
      x$children[[1]] <- shift_segments(x$children[[1]])
    } else {
      x$children[[1]] <- shift_paths(x$children[[1]])
    }
    x
  } else if (inherits(x, 'segments')) {
    shift_segments(x)
  } else {
    shift_paths(x)
  }
}

shift_segments <- function(x) {
  x0_new <- convertX(x$x0, 'mm', TRUE)
  x1_new <- convertX(x$x1, 'mm', TRUE)
  y0_new <- convertY(x$y0, 'mm', TRUE)
  y1_new <- convertY(x$y1, 'mm', TRUE)

  sep <- convertWidth(x$sep, 'mm', TRUE)

  xdiff <- x1_new - x0_new
  ydiff <- y1_new - y0_new
  lengths <- sqrt(xdiff * xdiff + ydiff * ydiff)
  xshift <- -ydiff / lengths * sep
  yshift <- xdiff / lengths * sep

  x$x0 <- unit(x0_new + xshift, 'mm')
  x$x1 <- unit(x1_new + xshift, 'mm')
  x$y0 <- unit(y0_new + yshift, 'mm')
  x$y1 <- unit(y1_new + yshift, 'mm')

  x
}

shift_paths <- function(x) {
  first <- which(!duplicated(x$id))
  last <- which(!duplicated(x$id, fromLast = TRUE))

  sep <- convertWidth(x$sep[first], 'mm', TRUE)

  x_new <- convertX(x$x, 'mm', TRUE)
  y_new <- convertY(x$y, 'mm', TRUE)
  xdiff <- x_new[last] - x_new[first]
  ydiff <- y_new[last] - y_new[first]
  lengths <- sqrt(xdiff * xdiff + ydiff * ydiff)
  xshift <- -ydiff / lengths * sep
  yshift <- xdiff / lengths * sep

  x$x <- unit(x_new + xshift[x$id], 'mm')
  x$y <- unit(y_new + yshift[x$id], 'mm')

  x
}
