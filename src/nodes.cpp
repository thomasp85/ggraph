#include "nodes.h"
std::vector<Node*> createHierarchy(std::vector<int> parent, std::vector<int> order, std::vector<double> weight) {
  std::vector<Node*> nodes;
  unsigned int i;
  for (i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], weight[i]);
    nodes.push_back(node);
  }
  for (i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}
std::vector<Node*> createHierarchy(std::vector<int> parent, std::vector<int> order, std::vector<double> weight, std::vector<double> height) {
  std::vector<Node*> nodes;
  unsigned int i;
  for (i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], weight[i], height[i]);
    nodes.push_back(node);
  }
  for (i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}

std::vector<Node*> createUnrooted(std::vector<int> parent, std::vector<int> order, std::vector<double> length) {
  std::vector<Node*> nodes;
  unsigned int i;
  for (i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], 1, length[i]);
    nodes.push_back(node);
  }
  for (i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}
