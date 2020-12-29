#' The class hierarchy of the flare visualization library
#'
#' This dataset contains the graph that describes the class hierarchy for the
#' [Flare](https://blokt.com/tool/prefuse-flare) ActionScript visualization library. It
#' contains both the class hierarchy as well as the import connections between
#' classes. This dataset has been used extensively in the D3.js documentation
#' and examples and are included here to make it easy to redo the examples in
#' ggraph.
#'
#' @format A list of three data.frames describing the software structure of
#' flare:
#' \describe{
#'  \item{edges}{This data.frame maps the hierarchical structure of the class
#'  hierarchy as an edgelist, with the class in `from` being the superclass
#'  of the class in `to`.}
#'  \item{vertices}{This data.frame gives additional information on the classes.
#'  It contains the full name, size and short name of each class.}
#'  \item{imports}{This data.frame contains the class imports for each class
#'  implementation. The `from` column gives the importing class and the
#'  `to` column gives the import.}
#' }
#'
#' @source The data have been adapted from the JSON downloaded from
#' <https://gist.github.com/mbostock/1044242#file-readme-flare-imports-json>
#' courtesy of Mike Bostock. The Flare framework is the work of the
#' [UC Berkeley Visualization Lab](http://vis.berkeley.edu/).
#'
'flare'
