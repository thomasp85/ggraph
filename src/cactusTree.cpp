#include "nodes.h"

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>

#include <vector>

void cactusTreeCircle(Node* node, double x, double y, double scale, double alpha, double span, double overlap) {
  Rectangle r = {x, y, std::pow(node->weight(), scale), 0.0};
  node->bounds = r;

  if (node->leaf()) return;

  std::vector<Node*> children = node->getChildren();

  std::sort(children.begin(), children.end(), [](Node* a, Node* b){return a->weight() < b->weight();});
  std::vector<Node *> ordered_children;
  double total_angle = 0.0;
  for (unsigned int i = 0; i < children.size(); i++) {
    total_angle += std::pow(children[i]->weight(), scale * (children.size() < 5 ? 2 : 0.75));
    ordered_children.insert(ordered_children.begin() + int(ordered_children.size() / 2), children[i]);
  }

  alpha -= span / 2;

  for (unsigned int i = 0; i < ordered_children.size(); i++) {
    double local_span = 0.5 * span * std::pow(ordered_children[i]->weight(), scale * (children.size() < 5 ? 2 : 0.75)) / total_angle;
    alpha += local_span;
    double child_r = std::pow(ordered_children[i]->weight(), scale);
    double dist = node->bounds.width + child_r * overlap;
    double x2 = x + dist * std::cos(alpha);
    double y2 = y + dist * std::sin(alpha);
    cactusTreeCircle(ordered_children[i], x2, y2, scale, alpha, 3.926991, overlap);
    alpha += local_span;
  }
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> cactusTree(cpp11::integers parent, cpp11::integers order, cpp11::doubles weight, double scale, double overlap, bool upright) {
  cpp11::writable::doubles_matrix<> circ(parent.size(), 3);
  unsigned int i;

  std::vector<Node*> nodes = createHierarchy(parent, order, weight);

  Node* startNode = nodes[0]->getRoot();
  startNode->tallyWeights();
  if (startNode->nChildren() == 1) {
    startNode = startNode->getChildren()[0];
  }
  double start_span = upright ? 3.926991 : 6.283185;
  cactusTreeCircle(startNode, 0.0, 0.0, scale, 1.570796, start_span, overlap);

  for (i = 0; i < nodes.size(); ++i) {
    circ(i, 0) = nodes[i]->bounds.x;
    circ(i, 1) = nodes[i]->bounds.y;
    circ(i, 2) = nodes[i]->bounds.width;
    delete nodes[i];
  }

  return circ;
}
