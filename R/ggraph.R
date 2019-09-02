#' Create a ggraph plot
#'
#' This function is the equivalent of [ggplot2::ggplot()] in ggplot2.
#' It takes care of setting up the plot object along with creating the layout
#' for the plot based on the graph and the specification passed in.
#' Alternatively a layout can be prepared in advance using
#' `create_layout` and passed as the data argument. See *Details* for
#' a description of all available layouts.
#'
#' @details
#' Following is a short description of the different layout types available in
#' ggraph. Each layout is further described in its own help pages. Any type of
#' regular graph/network data can be represented as a tbl_graph object. Because
#' of this the different layouts that can be applied to tbl_graph
#' objects are quite diverse, but not all layouts makes sense to all types of
#' graphs. It is up to the user to understand their data and choose an
#' appropriate layout. For standard node-edge diagrams igraph defines a
#' long range of different layout functions that are all available through the
#' `igraph` layout where the specific layout is specified using the
#' `algorithm` argument. In order to minimize typing all igraph algorithms
#' can also be passed directly into the `layout` argument.
#'
#' Any object that has an appropriate `as_tbl_graph` method can be passed
#' into `ggraph()` and will automatically be converted underneath.
#'
#' \describe{
#'   \item{`auto`}{The default layout. See [layout_tbl_graph_auto()] for further
#'   details}
#'   \item{`igraph`}{Use one of the internal igraph layout algorithms.
#'   The algorithm is specified using the `algorithm` argument. All strings
#'   accepted by the `algorithm` argument can also be supplied directly
#'   into `layout`. See [layout_tbl_graph_igraph()] for further
#'   details}
#'   \item{`dendrogram`}{Lays out the nodes in a tree-like graph as a
#'   dendrogram with leaves set at 0 and parents 1 unit above its tallest child.
#'   See [layout_tbl_graph_dendrogram()] for further details}
#'   \item{`manual`}{Lets the user manually specify the location of each
#'   node. See [layout_tbl_graph_manual()] for further details}
#'   \item{`linear`}{Arranges the nodes linearly or circularly in order to
#'   make an arc diagram. See [layout_tbl_graph_linear()] for further
#'   details}
#'   \item{`matrix`}{Arranges nodes on a diagonal thus preparing it for use with
#'   [geom_edge_point()] to make a matrix plot. See [layout_tbl_graph_matrix()]
#'   for further details}
#'   \item{`treemap`}{Creates a treemap from the graph, that is, a
#'   space-filing subdivision of rectangles showing a weighted hierarchy. See
#'   [layout_tbl_graph_treemap()] for further details}
#'   \item{`circlepack`}{Creates a layout showing a hierarchy as circles
#'   within circles. Conceptually equal to treemaps. See
#'   [layout_tbl_graph_circlepack()] for further details}
#'   \item{`partition`}{Create icicle or sunburst charts, where each layer
#'   subdivides the division given by the preceding layer. See
#'   [layout_tbl_graph_partition()] for further details}
#'   \item{`hive`}{Positions nodes on axes spreading out from the center
#'   based on node attributes. See [layout_tbl_graph_hive()] for further
#'   details}
#' }
#'
#' Alternatively a matrix or a data.frame can be provided to the `layout`
#' argument. In the former case the first column will be used as x coordinates
#' and the second column will by used as y coordinates, further columns are
#' dropped. In the latter case the data.frame is used as the layout table and
#' must thus contain a numeric x and y column.
#'
#' Lastly a function can be provided to the `layout` argument. It will be called
#' with the graph object as its first argument and any additional argument
#' passed into `ggraph()`/`create_layout()`. The function must return either a
#' data.frame or an object coercible to one and have an `x` and `y` column, or
#' an object coercible to a `tbl_graph`. In the latter case the node data is
#' extracted and used as layout (and must thus contain an `x` and `y` column)
#' and the graph will be added as the `graph` attribute.
#'
#' @param graph The object containing the graph. See *Details* for a list
#' of supported classes. Or a `layout_ggraph` object as returned from
#' `create_layout` in which case all subsequent arguments is ignored.
#'
#' @param layout The type of layout to create. Either a valid string, a
#' function, a matrix, or a data.frame (see Details)
#'
#' @param circular Should the layout be transformed into a radial
#' representation. Only possible for some layouts. Defaults to `FALSE`
#'
#' @param ... Arguments passed on to the layout function.
#'
#' @return For `ggraph()` an object of class gg onto which layers, scales,
#'   etc. can be added. For `create_layout()` an object inheriting from
#'   `layout_ggraph`. `layout_ggraph` itself inherits from
#'   `data.frame` and can be considered as such. The data.frame contains
#'   the node positions in the `x` and `y` column along with
#'   additional columns generated by the specific layout, as well as node
#'   parameters inherited from the graph. Additional information is stored as
#'   attributes to the data.frame. The original graph object is stored in the
#'   `graph` attribute and the `circular` attribute contains a logical
#'   indicating whether the layout has been transformed to a circular
#'   representation.
#'
#' @keywords layout network graph hierarchy visualisation
#'
#' @seealso [get_edges()] for extracting edge information from the
#' layout and [get_con()] for extracting path information.
#'
#' @examples
#' require(tidygraph)
#' gr <- create_notable('bull')
#' layout <- create_layout(gr, layout = 'igraph', algorithm = 'kk')
#' @export
#'
ggraph <- function(graph, layout = 'auto', ...) {
  envir <- parent.frame()
  p <- ggplot(data = create_layout(graph, layout, ...), environment = envir) +
    th_no_axes()
  class(p) <- c('ggraph', class(p))
  p
}
#' @export
ggplot_build.ggraph <- function(plot) {
  .register_graph_context(attr(plot$data, 'graph'), free = TRUE)
  NextMethod()
}
