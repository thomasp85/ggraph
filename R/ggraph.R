#' Create a ggraph plot
#'
#' This function is the equivalent of \code{\link[ggplot2]{ggplot}} in ggplot2.
#' It takes care of setting up the plot object along with creating the layout
#' for the plot based on the graph and the specification passed in.
#' Alternatively a layout can be prepared in advance using
#' \code{create_layout} and passed as the data argument. See \emph{Details} for
#' a desciption of all available layouts.
#'
#' @details
#' Following is a short description of the different layout types available in
#' ggraph. Each layout is further described in its own help pages.
#'
#' \strong{Dendrogram objects}
#'
#' The \code{\link[stats]{dendrogram}} class is used to store binary tree from
#' e.g. hierarchical clustering. The layouts provided for this class is
#' constrained to tree-like representations. \code{\link[stats]{hclust}} objects
#' are supported through automatic conversion to dendrogram objects and thus
#' supports the same layouts.
#'
#' \describe{
#'   \item{\code{auto}}{The default layout. Eqivalent to
#'   \code{layout = 'dendrogram'}}
#'   \item{\code{dendrogram}}{Creates a tree using the heights already defined in the
#'   dendrogram object. See \code{\link{layout_dendrogram_dendrogram}} for further details}
#'   \item{\code{even}}{Ignores the heights given by the dendrogram object and
#'   instead spreads the branch points out with an even distance. See
#'   \code{\link{layout_dendrogram_even}} for further details}
#' }
#'
#' Further, if the layouts provided for igraph objects are needed for dendrogram
#' objects \code{\link{den_to_igraph}} is provided to convert dendrograms to
#' igraph.
#'
#' \strong{igraph objects}
#'
#' Any type of regular graph/network data can be represented as an igraph
#' object. Because of this the different layouts that can be applied to igraph
#' objects are quite diverse, but not all layouts makes sense to all types of
#' graphs. It is up to the user to understand their data and choose an
#' appropriate layout. For standard node-edge diagrams igraph itself defines a
#' long range of different layout functions that are all available through the
#' \code{igraph} layout where the specific layout is specified using the
#' \code{algorithm} argument. In order to minimize typing all igraph algorithms
#' can also be passed directly into the \code{layout} argument.
#' \code{\link[network]{network}} objects are supported by automatic conversion
#' to igraph objects using \code{\link{network_to_igraph}} and thus supports the
#' same layouts.
#'
#' \describe{
#'   \item{\code{auto}}{The default layout. Equivalent to
#'   \code{layout = 'igraph', algorithm = 'nicely'}}
#'   \item{\code{igraph}}{Use one of the internal igraph layout algorithms.
#'   The algorithm is specified using the \code{algorithm} argument. All strings
#'   accepted by the \code{algorithm} argument can also be supplied directly
#'   into \code{layout}. See \code{\link{layout_igraph_igraph}} for further
#'   details}
#'   \item{\code{dendrogram}}{Lays out the nodes in a tree-like graph as a
#'   dendrogram with leaves set at 0 and parents 1 unit above its tallest child.
#'   See \code{\link{layout_igraph_dendrogram}} for further details}
#'   \item{\code{manual}}{Lets the user manually specify the location of each
#'   node by supplying a data.frame with an \code{x} and \code{y} column. See
#'   \code{\link{layout_igraph_manual}} for further details}
#'   \item{\code{linear}}{Arranges the nodes linearly or circularly in order to
#'   make an arc diagram. See \code{\link{layout_igraph_linear}} for further
#'   details}
#'   \item{\code{treemap}}{Creates a treemap from the graph, that is, a
#'   space-filing subdivision of rectangles showing a weighted hierarchy. See
#'   \code{\link{layout_igraph_treemap}} for further details}
#'   \item{\code{circlepack}}{Creates a layout showing a hierarchy as circles
#'   within circles. Conceptually equal to treemaps. See
#'   \code{\link{layout_igraph_circlepack}} for further details}
#'   \item{\code{partition}}{Create icicle or sunburst charts, where each layer
#'   subdivides the division given by the preceeding layer. See
#'   \code{\link{layout_igraph_partition}} for further details}
#'   \item{\code{hive}}{Positions nodes on axes spreading out from the center
#'   based on node attributes. See \code{\link{layout_igraph_hive}} for further
#'   details}
#' }
#'
#' @param graph The object containing the graph. See \emph{Details} for a list
#' of supported classes. Or a \code{layout_ggraph} object as returned from
#' \code{create_layout} in which case all subsequent arguments is ignored.
#'
#' @param layout The type of layout to create.
#'
#' @param circular Should the layout be transformed into a radial
#' representation. Only possible for some layouts. Defaults to \code{FALSE}
#'
#' @param ... Arguments passed on to the layout function.
#'
#' @return For \code{ggraph()} an object of class gg onto which layers, scales,
#'   etc. can be added. For \code{create_layout()} an object inherting from
#'   \code{layout_ggraph}. \code{layout_ggraph} itself inherits from
#'   \code{data.frame} and can be considered as such. The data.frame contains
#'   the node positions in the \code{x} and \code{y} column along with
#'   additional columns generated by the specific layout, as well as node
#'   parameters inherited from the graph. Additional information is stored as
#'   attributes to the data.frame. The original graph object is stored in the
#'   \code{graph} attribute and the \code{circular} attribute contains a logical
#'   indicating whether the layout has been transformed to a circular
#'   representation.
#'
#' @keywords layout network graph hierarchy visualisation
#'
#' @seealso \code{\link{get_edges}} for extracting edge information from the
#' layout and \code{\link{get_con}} for extracting path information.
#'
#' @examples
#' require(igraph)
#' gr <- make_graph('bull')
#' layout <- create_layout(gr, layout = 'igraph', algorithm = 'kk')
#'
#' @export
#'
ggraph <- function(graph, layout = 'auto', ...) {
    envir <- parent.frame()
    ggplot(data = create_layout(graph, layout, ...), environment = envir)
}
