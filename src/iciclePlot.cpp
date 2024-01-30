#include "nodes.h"

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>

#include <vector>

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

[[cpp11::register]]
cpp11::writable::doubles_matrix<> partitionTree(cpp11::integers parent, cpp11::integers order, cpp11::doubles weight, cpp11::doubles height) {
  cpp11::writable::doubles_matrix<> rect(parent.size(), 4);
  unsigned int i;

  std::vector<Node*> nodes = createHierarchy(parent, order, weight, height);

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
