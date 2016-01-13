nAngle <- function(x, y, degrees = TRUE) {
    angles <- atan(y/x)
    if (degrees) {
        angles*360/(2*pi)
    } else {
        angles
    }
}
eAngle <- function(x, y, xend, yend, degrees = TRUE) {
    x <- xend - x
    y <- yend - y
    nAngle(x, y, degrees)
}
