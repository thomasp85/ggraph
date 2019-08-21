#include <Rcpp.h>
using namespace Rcpp;

struct Rectangle {
  double x;
  double y;
  double width;
  double height;
};
class Node {
  std::vector<Node*> children;
  std::vector< std::vector<Node*> > allLeafs;
  Node* parent;
  bool hasParent;
  double Weight;
  double Height;
  int Id;
  int Order;

  static bool comparePtrToNode(Node* a, Node* b) {
    return (a->order() < b->order());
  }
public:
  Node() {
    hasParent = false;
    Weight = 0.0;
    Height = 0.0;
    Id = 0;
    Order = 0;
  };
  Node(int id, int order, double weight) {
    hasParent = false;
    Id = id;
    Order = order;
    Weight = weight;
  };
  Node(int id, int order, double weight, double height) {
    hasParent = false;
    Id = id;
    Order = order;
    Weight = weight;
    Height = height;
  };
  void rotate(double angle, double x, double y) {
    double s = sin(angle);
    double c = cos(angle);

    // translate point back to origin:
    double new_x = bounds.x - x;
    double new_y = bounds.y - y;

    // rotate point
    bounds.x = new_x * c - new_y * s + x;
    bounds.y = new_x * s + new_y * c + y;

    for (unsigned int i = 0; i < children.size(); ++i) {
      children[i]->rotate(angle, x, y);
    }
  };
  std::vector<Node*> getChildren() {
    std::vector<Node*> childVec;
    for (unsigned int i = 0; i < children.size(); ++i) {
      childVec.push_back(children[i]);
    }
    return childVec;
  };
  Node* getParent() {
    return parent;
  };
  std::vector<Node*> getLeafs() {
    std::vector<Node*> leafVec;
    collectLeafs(leafVec);
    return leafVec;
  };
  void collectLeafs(std::vector<Node*> &leafVec) {
    if (leaf()) {
      leafVec.push_back(this);
    } else {
      for (unsigned int i = 0; i < children.size(); ++i) {
        children[i]->collectLeafs(leafVec);
      }
    }
  };
  std::vector<Node*> getParentLeafs() {
    std::vector<Node*> leafVec;
    collectParentLeafs(leafVec);
    return leafVec;
  };
  void collectParentLeafs(std::vector<Node*> &leafVec) {
    if (orphan()) return;
    std::vector<Node*> siblings = parent->children;
    for (unsigned int i = 0; i < siblings.size(); ++i) {
      if (siblings[i] == this) continue;
      siblings[i]->collectLeafs(leafVec);
    }
    parent->collectParentLeafs(leafVec);
  };
  void collectAllLeafs() {
    if (orphan()) return;
    for (unsigned int i = 0; i < children.size(); ++i) {
      allLeafs.push_back(children[i]->getLeafs());
    }
    allLeafs.push_back(getParentLeafs());
  };
  std::vector< std::vector<Node*> > getAllLeafs() {
    if (allLeafs.empty()) {
      collectAllLeafs();
    }
    std::vector< std::vector<Node*> > leafs;

    for (unsigned int i = 0; i < allLeafs.size(); ++i) {
      std::vector<Node*> leafVec;
      for (unsigned int j = 0; j < allLeafs[i].size(); ++j) {
        leafVec.push_back(allLeafs[i][j]);
      }
      leafs.push_back(leafVec);
    }
    return leafs;
  }
  unsigned int nChildren() {
    return children.size();
  };
  unsigned int nOffspring() {
    unsigned int ret = nChildren();
    for (unsigned int i = 0; i < ret; ++i) {
      ret += children[i]->nOffspring();
    }
    return ret;
  };
  unsigned int nLeafs() {
    if (leaf()) return 1;
    unsigned int ret = 0;
    for (unsigned int i = 0; i < nChildren(); ++i) {
      if (children[i]->leaf()) {
        ret++;
      } else {
        ret += children[i]->nLeafs();
      }
    }
    return ret;
  }
  double weight() {
    return Weight;
  };
  double height() {
    return Height;
  };
  void addNode(Node* n) {
    double w = n->weight();
    Weight += w;
    if (hasParent) {
      parent->addWeight(w);
    }
    n->setParent(this);
    children.push_back(n);
  };
  void addWeight(double w) {
    if (hasParent) {
      parent->addWeight(w);
    }
    Weight += w;
  };
  void setParent(Node* n) {
    hasParent = true;
    parent = n;
  };
  bool orphan() {
    return !hasParent;
  };
  bool leaf() {
    return nChildren() == 0;
  }
  int order() {
    return Order;
  };
  void sortChildren() {
    std::sort(children.begin(), children.end(), comparePtrToNode);
  };
  Node* getRoot() {
    if (orphan()) {
      return this;
    } else {
      return parent->getRoot();
    }
  }

  Rectangle bounds;
};

std::vector<Node*> createHierarchy(std::vector<int> parent, std::vector<int> order, std::vector<double> weight);
std::vector<Node*> createHierarchy(std::vector<int> parent, std::vector<int> order, std::vector<double> weight, std::vector<double> height);
std::vector<Node*> createUnrooted(std::vector<int> parent, std::vector<int> order, std::vector<double> length);
