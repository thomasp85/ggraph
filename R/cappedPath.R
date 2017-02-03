cappedPathGrob <- function(x, y, id=NULL, id.lengths=NULL, rule="winding",
                           start.cap = NULL, start.cap2 = NULL, start.captype = 'circle',
                           end.cap = NULL, end.cap2 = NULL, end.captype = 'circle',
                           default.units="npc", name=NULL, gp=gpar(), vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (is.null(id) && !is.null(id.lengths)) {
        id <- rep(seq_along(id.lengths), id.lengths)
    }
    n <- length(unique(id))
    start.cap <- validateCap(start.cap, default.units, n)
    if (is.null(start.cap2)) {
        start.cap2 <- start.cap
    } else {
        start.cap2 <- validateCap(start.cap2, default.units, n)
    }
    end.cap <- validateCap(end.cap, default.units, n)
    if (is.null(end.cap2)) {
        end.cap2 <- end.cap
    } else {
        end.cap2 <- validateCap(end.cap2, default.units, n)
    }
    if (!all(c(start.captype, end.captype) %in% c('circle', 'rect')))
        stop('captype must be either `circle` or `rect`', call. = FALSE)
    start.captype <- rep(start.captype, length.out = n)
    end.captype <- rep(end.captype, length.out = n)
    if (is.null(start.cap) && is.null(end.cap)) {
        grob(x = x, y = y, id = id, id.lengths = NULL, rule = rule,
             name = name, gp = gp, vp = vp, cl = "pathgrob")
    } else {

        grob(x = x, y = y, id = id, id.lengths = NULL, rule = rule,
             start.cap = start.cap, start.cap2 = start.cap2, start.captype = start.captype,
             end.cap = end.cap, end.cap2 = end.cap2, end.captype = end.captype,
             name = name, gp = gp, vp = vp, cl = "cappedpathgrob")
    }
}

makeContent.cappedpathgrob <- function(x) {

}

validateCap <- function(cap, default.units, n) {
    if (is.null(cap)) return()
    if (!is.unit(cap)) cap <- unit(cap, default.units)
    rep(cap, length.out = n)
}
