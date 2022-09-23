#include <Rcpp.h>
using namespace Rcpp;

double max_leaf(NumericVector& x, LogicalVector& leaf) {
  double max = NA_REAL;
  for (int i = 0; i < x.length(); ++i) {
    if (leaf[i] && !NumericVector::is_na(x[i]) && (R_IsNA(max) || max < x[i])) {
      max = x[i];
    }
  }
  return max;
}

void recurse_dendrogram(ListOf<IntegerVector>& graph, int node, NumericVector& x, NumericVector& y, LogicalVector& leaf, double offset, bool repel, double pad, double ratio) {
  if (graph[node].length() == 0) {
    x[node] = offset;
  } else {
    double min_x = NA_REAL;
    double max_x = NA_REAL;
    for (int i = 0; i < graph[node].length(); ++i) {
      int child = graph[node][i] - 1;
      if (NumericVector::is_na(x[child])) {
        recurse_dendrogram(graph, child, x, y, leaf, offset, repel, pad, ratio);
        offset = max_leaf(x, leaf);
        if (repel) {
          offset += (REAL(y)[node] + pad) * ratio;
        } else {
          offset += 1 + pad;
        }
        if (R_IsNA(min_x) || x[child] < min_x) min_x = x[child];
        if (R_IsNA(max_x) || x[child] > max_x) max_x = x[child];
      }
    }
    x[node] = (min_x + max_x) / 2;
  }
}

//[[Rcpp::export]]
NumericVector dendrogram_spread(ListOf<IntegerVector> graph, IntegerVector starts, NumericVector y, LogicalVector leaf, bool repel, double pad, double ratio) {
  NumericVector x_loc(y.length(), NA_REAL);
  for (int i = 0; i < starts.length(); ++i) {
    recurse_dendrogram(graph, starts[i] - 1, x_loc, y, leaf, 0.0, repel, pad, ratio);
  }
  return x_loc;
}
