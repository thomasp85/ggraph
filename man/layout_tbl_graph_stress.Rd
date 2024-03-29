% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout_stress.R
\name{layout_tbl_graph_stress}
\alias{layout_tbl_graph_stress}
\alias{layout_tbl_graph_sparse_stress}
\title{Place nodes using stress majorisation}
\usage{
layout_tbl_graph_stress(
  graph,
  weights = NULL,
  niter = 500,
  tolerance = 1e-04,
  mds = TRUE,
  bbox = 50,
  x = NULL,
  y = NULL,
  circular = FALSE
)

layout_tbl_graph_sparse_stress(
  graph,
  pivots,
  weights = NULL,
  niter = 500,
  circular = FALSE
)
}
\arguments{
\item{graph}{a tbl_graph object}

\item{weights}{An expression evaluated on the edge data to provide edge
weights for the layout. Currently ignored for the sparse version}

\item{niter}{number of iterations during stress optimization}

\item{tolerance}{stopping criterion for stress optimization}

\item{mds}{should an MDS layout be used as initial layout (default: TRUE)}

\item{bbox}{constrain dimension of output. Only relevant to determine the
placement of disconnected graphs.}

\item{x, y}{Expressions evaluated on the node data giving
coordinates along x and/or y axis to fix nodes to. You can chose to only fix
selected nodes by leaving the remaining nodes with \code{NA} values.}

\item{circular}{ignored}

\item{pivots}{The number of pivot nodes.}
}
\value{
A data.frame with the columns \code{x}, \code{y}, \code{circular} as
well as any information stored as node variables in the tbl_graph object.
}
\description{
This layout is related to the stress-minimization algorithm known as
Kamada-Kawai (available as the 'kk' layout), but uses another optimization
strategy. It generally have better runtime, quality, and stability compared
to the Kamada-Kawai layout and is thus generally preferred. The sparse
version of the layout have better performance (especially on larger networks)
at the expense of layout quality, but will generally outperform many other
algorithms for large graphs in both runtime and quality (e.g. the 'drl'
layout from igraph).
}
\references{
Gansner, E. R., Koren, Y., & North, S. (2004). \emph{Graph drawing by stress
majorization.} In International Symposium on Graph Drawing (pp. 239-250). Springer, Berlin, Heidelberg.

Ortmann, M. and Klimenta, M. and Brandes, U. (2016). \emph{A Sparse Stress Model.} https://arxiv.org/pdf/1608.08909.pdf
}
\seealso{
Other layout_tbl_graph_*: 
\code{\link{layout_tbl_graph_auto}()},
\code{\link{layout_tbl_graph_backbone}()},
\code{\link{layout_tbl_graph_cactustree}()},
\code{\link{layout_tbl_graph_centrality}()},
\code{\link{layout_tbl_graph_circlepack}()},
\code{\link{layout_tbl_graph_dendrogram}()},
\code{\link{layout_tbl_graph_eigen}()},
\code{\link{layout_tbl_graph_fabric}()},
\code{\link{layout_tbl_graph_focus}()},
\code{\link{layout_tbl_graph_hive}()},
\code{\link{layout_tbl_graph_htree}()},
\code{\link{layout_tbl_graph_igraph}()},
\code{\link{layout_tbl_graph_linear}()},
\code{\link{layout_tbl_graph_manual}()},
\code{\link{layout_tbl_graph_matrix}()},
\code{\link{layout_tbl_graph_metro}()},
\code{\link{layout_tbl_graph_partition}()},
\code{\link{layout_tbl_graph_pmds}()},
\code{\link{layout_tbl_graph_sf}()},
\code{\link{layout_tbl_graph_treemap}()},
\code{\link{layout_tbl_graph_unrooted}()}
}
\author{
The underlying algorithm is implemented in the graphlayouts package
by David Schoch
}
\concept{layout_tbl_graph_*}
