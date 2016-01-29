![ggraph logo](inst/ggraph.png)

# ggraph
*/dʒiː.dʒɪˈrɑːf/*  (or g-giraffe)

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/ggraph.svg?branch=master)](https://travis-ci.org/thomasp85/ggraph)

### What it is
ggraph is an extension of ggplot2 tailored at plotting graph-like data 
structures (graphs, networks, trees, hierarchies...). It is not a graph plotting
framework that uses ggplot2 underneath, but rather an extension of the ggplot2 
API to make sense for graph data.

### State
ggraph is currently in beta stage development. There is still a lot of
implementations that needs to be done, as well as some interface quirks that 
needs to be decided on. Most importantly it currently relies on a [proposed 
change](https://github.com/hadley/ggplot2/pull/1486) to some ggplot2 internals
and unless these are merged in, I'm unsure how I will implement the API.

### Installation
As it relies on a still-unaccepted change to ggplot2, installation is at your
own risk (though it shouldn't blow up anything). If you just find this too 
compelling to pass on, do the following:

```r
if(!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github('thomasp85/ggplot2@function_as_data')
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/ggraph')
```

### Examples
Currently ggraph understands dendrogram and igraph objects. Others might be
added in the future (please file an issue if there is a particular class you
want supported), but until then see if your object is convertible into one
of the two.

#### Dendrogram
```r
# Let's use iris as we all love the iris dataset
## Perform hierarchical clustering on the iris data
irisDen <- as.dendrogram(hclust(dist(`rownames<-`(iris[1:4], iris[,5]), method='euclidean', ), method='ward.D2'))
## Add the species information to the leafs
irisDen <- dendrapply(irisDen, function(d) {
    if(is.leaf(d)) 
        attr(d, 'nodePar') <- list(species=iris[as.integer(attr(d, 'label')),5])
    d
})

# Plotting this looks very much like ggplot2 except for the new geoms
ggraph(graph = irisDen, layout = 'dendrogram', repel = TRUE, circular = TRUE, 
       ratio = 2) + 
    geom_edge_elbow() + 
    geom_node_text(aes(x = x*1.05, y=y*1.05, filter=leaf, 
                       angle = atan(y/x)*360/(2*pi), hjust='outward'), 
                   size=3) + 
    geom_node_point(aes(filter=leaf, color=species)) + 
    coord_fixed() + 
    ggforce::theme_no_axes()
```

![Dendrogram](https://dl.dropboxusercontent.com/u/2323585/ggraph/dendro1.png)

#### igraph
```r
# We'll just make up some data
library(igraph)
gr <- erdos.renyi.game(n=20, p=0.2)
E(gr)$weight <- sample(1:5, gsize(gr), TRUE)
V(gr)$class <- sample(letters[1:3], gorder(gr), TRUE)

# Plotting this is exactly as plotting a dendrogram
# All igraph layouts are available - here we use Fructerman and Reingold
ggraph(graph = gr, layout = 'fr') + 
    geom_edge_link(aes(size = weight), color = 'grey', alpha = 0.5) + 
    geom_node_point(aes(color = class), size = 10) + 
    coord_fixed() + 
    ggforce::theme_no_axes()
```

![Dendrogram](https://dl.dropboxusercontent.com/u/2323585/ggraph/hairball1.png)

### Scope
The plan is that ggraph should support all types of graph related visualization;
not just hairballs and trees. This means that treemaps, circle packing, 
sunburst, hive plots etc. will all find their place in ggraph. For a start I'll
draw inspiration from the vast library of graph visualizations available in 
D3.js, but if someone has a specific visualization approach they feel strongly 
for file an issue or a PR.

#### Roadmap
**Class support** *In order of importance*
- graph from package graph
- phylo from ape
- network from network
- data.tree from data.tree
- hypergraph from hypergraph (way down in the bottom along with all hypergraph
classes)

**Layouts**
- Treemap
- Sunburst / icicle
- Circle packing
- Hive plots
- Matrix plot
- Sankey diagram (maybe - undecided if it fits in ggraph)
- H-tree

**Connecions**
- Hierarchical Edge bundling (migrate from ggforce)
- geom_edge_trace
- geom_edge_connect

**geom_node_**
- density
- box

**geom_edge_**
- route (avoid node-edge collision)
- text
- tile (for matrix representations mainly)
- point (for matrix representations mainly)

**Other stuff**
- layout based on subset of edges
- Cut off edges before they reach node
