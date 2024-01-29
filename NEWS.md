# ggraph (development version)

* Fix a precision bug in circle pack layout (#345)
* `GeomEdgeTile` now uses `edge_width` instead of `edge_size` for the border
* Fix a bug in `geom_edge_parallel()` when used with edge labels. Edges are now
  correctly offset, though label may still overlap (#227)
* Fix a bug with auto layout on undirected trees (#247)
* Fix a bug in `get_con()` that would lead to wrong matching of additional data
  (#249)
* Allow `na.rm` in geoms (#301)
* If `check_overlap = TRUE` in `geom_node_text()` the rendering order is 
  reversed so that the top nodes are rendered, not the bottom ones
* Updated guides to the new ggplot2 interface. For `guide_edge_direction()`,
  this update adds the option to use labels instead of an arrow using the 
  `labels` argument.
* `create_layout()` now returns a modified tibble rather than a data.frame
* The `...` in `get_nodes()`, `get_edges()`, and `get_con()` now accepts 
  tidy evaluation. `get_nodes()` and `get_con()` will get evaluated on the node
  data in the original order (layouts may reorder nodes), and `get_edges()` will
  get evaluated on the edge data (#272)
* The linear layout has gained a `weight` argument allowing you to set how much
  space each node occupy. Further, the layout now calculates statistics so that
  it can be used in conjunction with `geom_node_tile()` and `geom_node_arc_bar()`
* New layout added. Cactustree is a hierarchical layout optimised for 
  hierarchical edge bundling by placing nodes as budding circles on the 
  periphery of their parent (#226)
* Updated layout functions from the graphlayouts package to support grouped and
  constrained versions (centrality, focus, and stress layouts)
* Added H Tree layout for binary trees (#58)
* Added `geom_edge_bundle_force()`, `geom_edge_bundle_path()`, and 
  `geom_edge_bundle_minimal()` (+ variants) to provide support for edge bundling 
  (#267)
* Add "metro" layout from graphlayouts for metroline like layouts

# ggraph 2.1.0

* Update errors to use cli
* Use vctrs internally
* Add binned edge scales
* Fix a bug where removing the full data with a filter would throw an error
* Make sure all lyaout functions now correctly merge the layout with the node
  data without getting duplicate column names
* Fixed a bug in the start_cap implementation that would cause edges consisting
  of only two points to disappear
* Fixed a bug in `facet_graph()` when using `.` on either dimension (#313)
* Move dendrogram layout calculations to compiled code to avoid R's recursion
  stack limitations (#311)
* Fixed a bug in the circle pack algorithm that could result in overlapping 
  circles (#305)

# ggraph 2.0.6

* Fix vignette errors on CRAN

# ggraph 2.0.5

* Fix deprecation of std::random_shuffle in C++14 by specifyin C++11 as the
  compile version

# ggraph 2.0.4

* Fix bug in faceting functions where using algorithms based on sampling 
  directly would result in errors
* Move CI to GitHub Actions
* Warn if a precalculated layout is passed to `layout` in `create_layout()` 
  (#208)
* Warn when setting `dim` to a value higher than 2 in igraph layouts (#160)
* Fix bug when trying to plot a graph with no edges (#214)
* Fix a bug in edge geoms due to a typo (#246)
* Fix a bug in edge geoms where setting `label_colour` to `NA` would turn it off
  instead of inheriting the colour of the edge (#238)
* Fix a bug in edge geoms that prevented edge labels from being parsed as 
  expressions (#245)
* Improve performance of dendrogram layout when calculating depth (#248)
* Fix a bug in hive layout when segmenting by a factor with unused levels (#252)
* Fix a bug in capping of path due to updates in grid. Setting cap unit to 
  `native` now works as expected again (#254)
* Fix a bug in `get_con()` that would require input to be sorted in the `from`
  vector (#258)

# ggraph 2.0.3

* Fix bug in `geom_edge_elbow()` that resulted in duplicated `group` columns in
  the data.

# ggraph 2.0.2

* Remove reshape2 dependency

# ggraph 2.0.1

* Fix bug in ggraph that will surface with the next grid release
* Deprecate `qgraph()` in favour of `autograph()` to avoid name collision with
  `qgraph::qgraph()`. `autograph()` is now also a generic with a default method,
  so you can provide your own specific ggraph plot method for your network 
  classes

# ggraph 2.0.0

This release is a major release including many new features, bug fixes and some
breaking changes.

## Breaking changes
* Use tidygraph as the central data format. The results of this are several:

  - All graph object supported by tidygraph are now supported on even footing in
    ggraph. All layouts are now available to any graph class
  - **BREAKING** The `"even"` layout for dendrograms are no more, but can be
    obtained by using the `"dendrogram"` layout with `height = NULL`
  - **BREAKING** All layouts uses NSE for arguments that refer to node and edge
    variables, instead of passing in strings that refer to the variable name.
  - All examples and vignettes now uses tidygraph for graph manipulation 
    resulting in much cleaner code.
  - `tree_apply` has been removed in favour of using `tidygraph::map_bfs_*`
  - `geom_edge_elbow` is no longer only available to dendrogram objects
  - tidygraph algorithms can now be used directly within ggraph functions. E.g.
    you can have `sort.by = node_rank_hclust()` in your specification of a 
    linear layout, or `aes(colour = group_infomap())` in node geoms
  
  This big change fixes #21, #72, #79, and #81. A vignette has been added to 
  describe the integration in more detail
* The `curvature` argument from `geom_edge_arc()` and `geom_edge_hive()` as well 
  as the `spread` argument from `geom_edge_fan()` has been deprecated in favor 
  of the new `strength` argument.
* ggraph plots now gets constructed with grid and axes removed from the default
  theme.

## New features

* Add equal-angle and equal-daylight unrooted tree layouts (#59)
* Interface with the graphlayouts package and make all its layouts available. 
  The auto layout now uses the stress layout for standard graphs (sparse stress)
  for larger graphs.
* Added length argument to the dendrogram layout to allow the layout to be based
  on edge length rather than node heights (#124).
* Added `geom_edge_parallel()` for drawing multiedges as parallel lines (#191)
* Added fabric layout to create biofabric plots. Also added `geom_node_range()`
  and `geom_edge_span()` for visualising such layouts (#47)
* Added `geom_edge_bend()` for drawing soft elbows (#45)
* Added `geom_node_voronoi()` for displaying nodes as voronoi tiles (#100)
* Added `qgraph()` for quickly creating a standard network plot for explorative
  purpose (#94)
* All non-straight line-based edge geoms now has a `strength` parameter that
  controls their deviation from a straight line. `0` will always give a straight
  line while `1` will be their natural look. Numbers outside this range may look
  weird (#97)
* Added `weight` and `mode` arguments to `get_con()` that are passed on to the
  shortest path calculations (#89).
* Add matrix layout and `geom_edge_point()` (#23)
* Added `geom_edge_tile()` for use with matrix layouts (#141)
* Manual layouts are now easier to specify. You can pass a matrix or data.frame
  to the layout parameter that will then be used. Also, if an `x` and `y` 
  argument is present, the `auto` layout will choose the `manual` layout. (#91)
* Custom layout functions are now much more flexible. And can either return a
  `data.frame` or an object coercible to a `tbl_graph`. In the latter case the
  node table will be used as layout and the graph will be attached. This allows
  direct use of the particles package as a layout engine as a side effect. (#88)
  
## Bug fixes

* Fixed numerous bugs and issues pertaining to the group aesthetic handling in
  many edge geoms (#190, #193).
* Edge geoms no longer throws an error when all edges are completely capped 
  (#176)
* Fixed a bug that prevented edge capping from being used with *2 variants of
  edge geoms (#167)
* Fix bug in edge capping that could lead to edges extending to (0,0) (#163)
* Fix bug affecting faceting of capped edges (#140)
* Character aesthetics are no longer cast to factors in edge geoms (#131)
* Fix a bug where start capping was ignored if the previous edge had been
  completely removed by capping (#150)
* Fix offsetting bug in edge label drawing when some labels are empty strings 
  and `label_parse = TRUE` (#159)
* Fixed a bug resulting in the wrong mapping of additional values to connections
  (#122, #134)
* Fixed a bug when using `facet_graph()` with data from both tbl_df and 
  data.frame
* Added `override.aes` to `guide_edge_direction()`
* Fix bug with extracting edges from an empty graph (#76)
* Fixed bug causing shifts in edge aesthetics when one or more edges were 
  completely clipped (#62, #115)
* Fixed bug when clipping completely orthogonal points that produced additional
  lines going to (0,0) (#70, #84, #103)

## Other

* Changed license to MIT
* Update roxygen documentation to use markdown and reduce duplication (#95)
