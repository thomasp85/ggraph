#include "nodes.h"

#include <vector>

std::vector<Node*> createHierarchy(cpp11::integers parent, cpp11::integers order, cpp11::doubles weight) {
  std::vector<Node*> nodes;
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], weight[i]);
    nodes.push_back(node);
  }
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}

std::vector<Node*> createHierarchy(cpp11::integers parent, cpp11::integers order, cpp11::doubles weight, cpp11::doubles height) {
  std::vector<Node*> nodes;
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], weight[i], height[i]);
    nodes.push_back(node);
  }
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}

std::vector<Node*> createHierarchy(cpp11::integers parent, cpp11::integers order) {
  std::vector<Node*> nodes;
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], 0.0);
    nodes.push_back(node);
  }
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}

std::vector<Node*> createUnrooted(cpp11::integers parent, cpp11::integers order, cpp11::doubles length) {
  std::vector<Node*> nodes;
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    Node* node = new Node(i, order[i], 1, length[i]);
    nodes.push_back(node);
  }
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}
