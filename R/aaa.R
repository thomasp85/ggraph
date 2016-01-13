#' @importFrom ggplot2 ggproto StatIdentity
#' @export
StatFilter <- ggproto('StatFilter', StatIdentity,
    compute_layer = function(data, scales, params) {
        data[data$filter, ]
    }
)

aesIntersect <- function(aes1, aes2) {
    structure(
        c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
        class = 'uneval'
    )
}
