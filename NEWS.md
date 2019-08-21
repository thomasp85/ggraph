# ggraph (development version)

* Add equal-angle and equal-daylight unrooted tree layouts (#59)
* Interface with the graphlayouts package and make all its layouts available. 
  The auto layout now uses the stress layout for standard graphs (sparse stress)
  for larger graphs.
* Fixed numerious bugs and issues pertaining to the group aesthetic handling in
  many edge geoms (#190, #193).
* Added length argument to the dendrogram layout to allow the layout to be based
  on edge length rather than node heights (#124).
* Added `geom_edge_parallel()` for drawing multiedges as parallel lines (#191)
* Added fabric layout to create biofabric plots. Also added `geom_node_range()`
  and `geom_edge_span()` for visualising such layouts (#47)
* Added `geom_edge_bend()` for drawing soft elbows (#45)
* Added `geom_node_voronoi()` for displaying nodes as voronoi tiles (#100)
* All non-straight line-based edge geoms now has a `strength` parameter that
  controls their deviation from a straight line. `0` will always give a straight
  line while `1` will be their natural look. Numbers outside this range may look
  weird. This also deprecates the `curvature` argument from `geom_edge_arc()` 
  and `geom_edge_hive()` as well as the `spread` argument from `geom_edge_fan()`
  (#97)
* Added `qgraph()` for quickly creating a standard network plot for explorative
  purpose (#94)
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
* Added `weight` and `mode` arguments to `get_con()` that are passed on to the
  shortest path calculations (#89).
* Fixed a bug resulting in the wrong mapping of additional values to connections
  (#122, #134)
* Fixed a bug when using `facet_graph()` with data from both tbl_df and 
  data.frame
* Added `override.aes` to `guide_edge_direction()`
* Changed license to MIT
* Add matrix layout and `geom_edge_point()` (#23)
* Added `geom_edge_tile()` for use with matrix layouts (#141)
* Fix bug with extracting edges from an empty graph (#76)
* Update roxygen documentation to use markdown and reduce duplication (#95)
* Manual layouts are now easier to specify. You can pass a matrix or data.frame
  to the layout parameter that will then be used. Also, if an `x` and `y` 
  argument is present, the `auto` layout will choose the `manual` layout. (#91)
* Custom layout functions are now much more flexible. And can either return a
  `data.frame` or an object coercible to a `tbl_graph`. In the latter case the
  node table will be used as layout and the graph will be attached. This allows
  direct use of the particles package as a layout engine as a side effect. (#88)
* Fixed bug causing shifts in edge aesthetics when one or more edges were 
  completely clipped (#62, #115)
* Fixed bug when clipping completely orthogonal points that produced additional
  lines going to (0,0) (#70, #84, #103)
* Use tidygraph as the central data format. The results of this are several:

  - All graph object supported by tidygraph are now supported on even footing in
    ggraph. All layouts are now available to any graph class
  - **BREAKING** The `"even"` layout for dendrograms are no more, but can be
    obtained by using the `"dendrogram"` layout with `height = NULL`
  - **BREAKING** All layouts uses NSE for arguments that refer to node and edge
    variables, instead of passing in strings that refer to the variable name.
  - All examples and vignettes now uses tidygraph for graph manipulation 
    resulting in much cleaner code.
  - `tree_apply` has been deprecated in favour of using `tidygraph::map_bfs_*`
  - `geom_edge_elbow` is no longer only available to dendrogram objects
  - tidygraph algorithms can now be used directly within ggraph functions. E.g.
    you can have `sort.by = node_rank_hclust()` in your specification of a 
    linear layout, or `aes(colour = group_infomap())` in node geoms
  
  This big change fixes #21, #72, #79, and #81. A vignette has been added to 
  describe the integration in more detail
