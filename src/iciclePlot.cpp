#include <Rcpp.h>
#include "nodes.h"
using namespace Rcpp;

void icicleLayout(Node* node, double x, double y) {
  Rectangle r = {x, y, node->weight(), node->height()};
  node->bounds = r;
  std::vector<Node*> children = node->getChildren();
  if (children.size() != 0) {
    y += node->height();
    for (unsigned int i = 0; i < children.size(); i++) {
      icicleLayout(children[i], x, y);
      x += children[i]->weight();
    }
  }
}

//[[Rcpp::export]]
NumericMatrix partitionTree(IntegerVector parent, IntegerVector order, NumericVector weight, NumericVector height) {
  NumericMatrix rect(parent.size(), 4);
  unsigned int i;

  std::vector<Node*> nodes = createHierarchy(as< std::vector<int> >(parent), as< std::vector<int> >(order), as< std::vector<double> >(weight), as< std::vector<double> >(height));

  for (i = 0; i < nodes.size(); ++i) {
    nodes[i]->sortChildren();
  }

  Node* startNode = nodes[0]->getRoot();
  icicleLayout(startNode, 0, 0);

  for (i = 0; i < nodes.size(); ++i) {
    rect(i, 0) = nodes[i]->bounds.x;
    rect(i, 1) = nodes[i]->bounds.y;
    rect(i, 2) = nodes[i]->bounds.width;
    rect(i, 3) = nodes[i]->bounds.height;
    delete nodes[i];
  }

  return rect;
}
