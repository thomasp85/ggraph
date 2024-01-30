#include <cpp11/logicals.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/list_of.hpp>

double max_leaf(cpp11::doubles& x, cpp11::logicals& leaf) {
  double max = NA_REAL;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (leaf[i] && !cpp11::is_na(x[i]) && (R_IsNA(max) || max < x[i])) {
      max = x[i];
    }
  }
  return max;
}

void recurse_dendrogram(cpp11::list_of<cpp11::integers>& graph, int node, cpp11::writable::doubles& x, cpp11::doubles& y, cpp11::logicals& leaf, double offset, bool repel, double pad, double ratio) {
  if (graph[node].size() == 0) {
    x[node] = offset;
  } else {
    double min_x = NA_REAL;
    double max_x = NA_REAL;
    for (int i = 0; i < graph[node].size(); ++i) {
      int child = graph[node][i] - 1;
      if (R_IsNA(x[child])) {
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

[[cpp11::register]]
cpp11::writable::doubles dendrogram_spread(cpp11::list_of<cpp11::integers> graph, cpp11::integers starts, cpp11::doubles y, cpp11::logicals leaf, bool repel, double pad, double ratio) {
  cpp11::writable::doubles x_loc(y.size());
  std::fill(x_loc.begin(), x_loc.end(), NA_REAL);
  for (R_xlen_t i = 0; i < starts.size(); ++i) {
    recurse_dendrogram(graph, starts[i] - 1, x_loc, y, leaf, 0.0, repel, pad, ratio);
  }
  return x_loc;
}
