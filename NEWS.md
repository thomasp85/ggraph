# ggraph (development version)

* Character aesthetics are no longer cast to factors in edge geoms (#131)
* Added `weight` and `mode` arguments to `get_con()` that are passed on to the
  shortest path calculations (#89).
* Fixed a bug resulting in the wrong mapping of additional values to connections
  (#122, #134)
* Fixed a bug when using `facet_graph()` with data from both tbl_df and 
  data.frame
* Added `override.aes` to `guide_edge_direction()`
* Changed license to MIT
* Add matrix layout and `geom_edge_point()` (#23)
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
