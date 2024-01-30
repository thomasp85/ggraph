#include "nodes.h"

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>

#include <vector>

void hLayout(Node* node, double x, double y, bool horizontal, double length) {
  Rectangle r = {x, y, 0.0, 0.0};
  node->bounds = r;
  std::vector<Node*> children = node->getChildren();
  double next_length = length / 1.414214;
  for (unsigned int i = 0; i < children.size(); i++) {
    length *= -1;
    hLayout(children[i], horizontal ? x + length : x, horizontal ? y : y + length, !horizontal, next_length);
  }
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> hTree(cpp11::integers parent, cpp11::integers order) {
  cpp11::writable::doubles_matrix<> pos(parent.size(), 2);
  unsigned int i;

  std::vector<Node*> nodes = createHierarchy(parent, order);

  for (i = 0; i < nodes.size(); ++i) {
    nodes[i]->sortChildren();
  }

  Node* startNode = nodes[0]->getRoot();
  hLayout(startNode, 0, 0, false, 1.0);

  for (i = 0; i < nodes.size(); ++i) {
    pos(i, 0) = nodes[i]->bounds.x;
    pos(i, 1) = nodes[i]->bounds.y;
    delete nodes[i];
  }

  return pos;
}
