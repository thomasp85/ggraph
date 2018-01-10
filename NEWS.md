# ggraph 1.0.0.9999

* Fixed a bug when using `facet_graph()` with data from both tbl_df and 
  data.frame
* Added `override.aes` to `guide_edge_direction()`
* Changed license to MIT
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
  
  This big change fixes #21, #72, #79, and #81
